#include <algorithm>
#include <numeric>
#include <random>
#include <span>

#include <cstddef>

#include "code_edsl.h"

#include <SDL2/SDL.h>

#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include <cassert>

using namespace edsl;

// ROMS:
// https://github.com/ColinEberhardt/wasm-rust-chip8/tree/master/web/roms
// Docs:
// http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
// https://github.com/JamesGriffin/CHIP-8-Emulator
// https://blog.scottlogic.com/2017/12/13/chip8-emulator-webassembly-rust.html

static constexpr std::uint8_t kDisplayWidth = 64;
static constexpr std::uint8_t kDisplayHeight = 32;

struct Chip8
{
    std::uint8_t memory_[4 * 1024];
    std::uint8_t V_[16];        // Vx, where x is [0; F]. VF should not be used.
    std::uint16_t stack_[16];   // Stack with `sp_`.
    std::uint16_t pc_;          // Program counter.
    std::uint16_t I_;           // Index register, 12 bits.
    std::uint8_t delay_timer_;  // Decremented at a rate of 60Hz.
    std::uint8_t sp_;           // Stack Pointer.
    std::uint8_t sound_timer_;  // Only one tone.
    bool keys_[16];             // 0-F (16) keys.
    std::uint8_t display_memory_[kDisplayWidth][kDisplayHeight];

    std::random_device* rd_;
    bool needs_redraw_;   // For "optimizations".
    bool waits_keyboard_;

    explicit Chip8(std::random_device& rd)
        : memory_()
        , V_()
        , stack_()
        , pc_(0)
        , I_(0)
        , delay_timer_(0)
        , sp_(0)
        , sound_timer_(0)
        , keys_()
        , display_memory_()
        , rd_(&rd)
        , needs_redraw_(false)
        , waits_keyboard_(false)
    {
    }

    ~Chip8() = default;
    Chip8(const Chip8&) = delete;
    Chip8& operator=(const Chip8&) = delete;
    Chip8(Chip8&&) = delete;
    Chip8& operator=(Chip8&&) = delete;

    void boot_up();
    bool draw(std::uint8_t x, std::uint8_t y
        , std::span<const std::uint8_t> sprite);
    void clear_display();
    void wait_any_key(std::uint8_t vindex);
    void execute_cycle();
    void execute_opcode(std::uint16_t opcode);
};

constexpr std::uint8_t kHexDigitsSprites[] =
{
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
};

static std::uint8_t overflow_add(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& carry);
static std::uint8_t overflow_sub(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& borrow);
static std::uint8_t random_byte(std::random_device& rd);

void Chip8::execute_cycle()
{
    const std::uint16_t opcode = std::uint16_t(
        (memory_[pc_] << 8) | (memory_[pc_ + 1]));
    
    pc_ += 2;
    execute_opcode(opcode);

    // Note: wrong. Should be decreased at 60 Hz speed.
    if (delay_timer_ > 0)
    {
        --delay_timer_;
    }
    if (sound_timer_ > 0)
    {
        --sound_timer_;
    }
}

void Chip8::execute_opcode(std::uint16_t opcode)
{
    match_opcode(*this, opcode
        , [](Chip8&) { assert(false && "Unknown opcode."); }
        , code(0x0, 0x0, 0xE, 0x0, [](Chip8& self)
        { /*CLS*/
            self.clear_display(); })
        , code(0x0, 0x0, 0xE, 0xE, [](Chip8& self)
        { /*RET*/
            assert(self.sp_ > 0);
            --self.sp_;
            assert(self.sp_ < std::size(self.stack_));
            self.pc_ = self.stack_[self.sp_]; })
        , code(0x0, _n, _n, _n, [](Chip8&, std::uint16_t)
        { /* SYS addr. This instruction is only used on the old computers
            on which Chip-8 was originally implemented.
            It is ignored by modern interpreters. */ })
        , code(0x1, _n, _n, _n, [](Chip8& self, std::uint16_t nnn)
        { /* JP addr. */
            self.pc_ = nnn; })
        , code(0x2, _n, _n, _n, [](Chip8& self, std::uint16_t nnn)
        { /* CALL addr. */
            assert(self.sp_ < std::size(self.stack_));
            self.stack_[self.sp_] = self.pc_;
            ++self.sp_;
            self.pc_ = nnn; })
        , code(0x3, _x, _k, _k, [](Chip8& self, std::uint8_t x, std::uint8_t kk)
        { /* SE Vx, byte. */
            self.pc_ += ((self.V_[x] == kk) ? 2 : 0); })
        , code(0x4, _x, _k, _k, [](Chip8& self, std::uint8_t x, std::uint8_t kk)
        { /* SNE Vx, byte. */
            self.pc_ += ((self.V_[x] != kk) ? 2 : 0); })
        , code(0x5, _x, _y, 0x0, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* SE Vx, Vy. */
            self.pc_ += ((self.V_[x] == self.V_[y]) ? 2 : 0); })
        , code(0x6, _x, _k, _k, [](Chip8& self, std::uint8_t x, std::uint8_t kk)
        { /* LD Vx, byte. */
            self.V_[x] = kk; })
        , code(0x7, _x, _k, _k, [](Chip8& self, std::uint8_t x, std::uint8_t kk)
        { /* ADD Vx, byte. */
            self.V_[x] += kk; })
        , code(0x8, _x, _y, 0x0, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* LD Vx, Vy. */
            self.V_[x] = self.V_[y]; })
        , code(0x8, _x, _y, 0x1, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* OR Vx, Vy. */
            self.V_[x] = self.V_[x] | self.V_[y]; })
        , code(0x8, _x, _y, 0x2, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* AND Vx, Vy. */
            self.V_[x] = self.V_[x] & self.V_[y]; })
        , code(0x8, _x, _y, 0x3, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* XOR Vx, Vy. */
            self.V_[x] = self.V_[x] ^ self.V_[y]; })
        , code(0x8, _x, _y, 0x4, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* ADD Vx, Vy. */
            self.V_[x] = overflow_add(self.V_[x], self.V_[y], self.V_[0xF]); })
        , code(0x8, _x, _y, 0x5, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* SUB Vx, Vy. */
            self.V_[x] = overflow_sub(self.V_[x], self.V_[y], self.V_[0xF]);
            self.V_[0xF] = ((self.V_[0xF] == 0) ? 1 : 0); })
        , code(0x8, _x, _, 0x6, [](Chip8& self, std::uint8_t x)
        { /* SHR Vx {, Vy}. */
            self.V_[0xF] = self.V_[x] & 0x1;
            self.V_[x] >>= 1; })
        , code(0x8, _x, _y, 0x7, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* SUBN Vx, Vy. */
            self.V_[x] = overflow_sub(self.V_[y], self.V_[x], self.V_[0xF]);
            self.V_[0xF] = ((self.V_[0xF] == 0) ? 1 : 0); })
        , code(0x8, _x, _, 0xE, [](Chip8& self, std::uint8_t x)
        { /* SHL Vx {, Vy}. */
            self.V_[0xF] = self.V_[x] & 0x80;
            self.V_[x] <<= 1; })
        , code(0x9, _x, _y, 0x0, [](Chip8& self, std::uint8_t x, std::uint8_t y)
        { /* SNE Vx, Vy. */
            self.pc_ += ((self.V_[x] != self.V_[y]) ? 2 : 0); })
        , code(0xA, _n, _n, _n, [](Chip8& self, std::uint16_t nnn)
        { /* LD I, addr. */
            self.I_ = nnn; })
        , code(0xB, _n, _n, _n, [](Chip8& self, std::uint16_t nnn)
        { /* JP V0, addr. */
            self.pc_ = std::uint16_t(nnn + self.V_[0]); })
        , code(0xC, _x, _k, _k, [](Chip8& self, std::uint8_t x, std::uint8_t kk)
        { /* RND Vx, byte. */
            self.V_[x] = random_byte(*self.rd_) & kk; })
        , code(0xD, _x, _y, _n, [](Chip8& self, std::uint8_t x, std::uint8_t y, std::uint8_t n)
        { /* DRW Vx, Vy, nibble. */
            const bool collision = self.draw(self.V_[x], self.V_[y]
                , {self.memory_ + self.I_, n});
            self.V_[0xF] = (collision ? 1 : 0);
            self.needs_redraw_ = true; })
        , code(0xE, _x, 0x9, 0xE, [](Chip8& self, std::uint8_t x)
        { /* SKP Vx. */
            self.pc_ += (self.keys_[self.V_[x]] ? 2 : 0); })
        , code(0xE, _x, 0xA, 0x1, [](Chip8& self, std::uint8_t x)
        { /* SKNP Vx. */
            self.pc_ += (!self.keys_[self.V_[x]] ? 2 : 0); })
        , code(0xF, _x, 0x0, 0x7, [](Chip8& self, std::uint8_t x)
        { /* LD Vx, DT. */
            self.V_[x] = self.delay_timer_; })
        , code(0xF, _x, 0x0, 0xA, [](Chip8& self, std::uint8_t x)
        { /* LD Vx, K. */
            self.wait_any_key(x); })
        , code(0xF, _x, 0x1, 0x5, [](Chip8& self, std::uint8_t x)
        { /* LD DT, Vx. */
            self.delay_timer_ = self.V_[x]; })
        , code(0xF, _x, 0x1, 0x8, [](Chip8& self, std::uint8_t x)
        { /* LD ST, Vx. */
            self.sound_timer_ = self.V_[x]; })
        , code(0xF, _x, 0x1, 0xE, [](Chip8& self, std::uint8_t x)
        { /* ADD I, Vx. */
            self.I_ += self.V_[x]; })
        , code(0xF, _x, 0x2, 0x9, [](Chip8& self, std::uint8_t x)
        { /* LD F, Vx. */
            const auto sprite_bytes = (std::size(kHexDigitsSprites) / 16);
            self.I_ = std::uint16_t(self.V_[x] * sprite_bytes); })
        , code(0xF, _x, 0x3, 0x3, [](Chip8& self, std::uint8_t x)
        { /* LD B, Vx. */
            std::uint8_t* ptr = (self.memory_ + self.I_);
            ptr[0] = (self.V_[x] / 100);
            ptr[1] = (self.V_[x] / 10) % 10;
            ptr[2] = (self.V_[x] % 100) % 10; })
        , code(0xF, _x, 0x5, 0x5, [](Chip8& self, std::uint8_t x)
        { /* LD [I], Vx. */
            std::copy(self.V_, self.V_ + x + 1
                , self.memory_ + self.I_); })
        , code(0xF, _x, 0x6, 0x5, [](Chip8& self, std::uint8_t x)
        { /* LD Vx, [I]. */
            std::copy(self.memory_ + self.I_, self.memory_ + self.I_ + x + 1
                , self.V_); })
        );

    assert(pc_ < std::size(memory_));
}

template<typename T, typename ReduceOp, typename TransformOp>
constexpr T outer_index_product(std::uint8_t rows, std::uint8_t columns
    , T init
    , ReduceOp reduce
    , TransformOp transform)
{
    for (std::uint8_t r  = 0; r < rows; ++r)
    {
        for (std::uint8_t c = 0; c < columns; ++c)
        {
            init = reduce(std::move(init), transform(r, c));
        }
    }
    return init;
}

bool Chip8::draw(std::uint8_t x, std::uint8_t y
    , std::span<const std::uint8_t> sprite)
{
    auto bit_at = [](std::uint8_t value, std::uint8_t index)
    {
        assert(index < 8);
        return std::uint8_t((value >> (7 - index)) & 0x01u);
    };

    struct DrawInput
    {
        const std::uint8_t new_pixel;
        const std::uint8_t x;
        const std::uint8_t y;
    };

    return outer_index_product(std::uint8_t(sprite.size()), 8, false
        , [this](bool prev, DrawInput input)
    {
        std::uint8_t& old_pixel = display_memory_[input.x][input.y];
        bool collision = false;
        if (input.new_pixel == 1)
        {
            collision = (old_pixel == 1);
            old_pixel = (input.new_pixel ^ old_pixel);
        }
        return (prev || collision);
    }
        , [&](std::uint8_t r, std::uint8_t c)
    {
        std::uint8_t new_value = bit_at(sprite[r], c);
        std::uint8_t x_wrap = (x + c) % kDisplayWidth;
        std::uint8_t y_wrap = (y + r) % kDisplayHeight;
        return DrawInput{new_value == 1, x_wrap, y_wrap};
    });
}

void Chip8::boot_up()
{
    pc_ = 0x200;
    needs_redraw_ = true;
    std::copy(std::begin(kHexDigitsSprites), std::end(kHexDigitsSprites), memory_);
}

void Chip8::clear_display()
{
    std::fill(&display_memory_[0][0]
        , &display_memory_[0][0] + sizeof(display_memory_)
        , std::uint8_t(0));
}

void Chip8::wait_any_key(std::uint8_t vindex)
{
    pc_ -= 2; // busy wait.
    waits_keyboard_ = true;
    auto it = std::find(std::cbegin(keys_), std::cend(keys_), true);
    if (it == std::cend(keys_))
    {
        return;
    }
    V_[vindex] = std::uint8_t(it - std::cbegin(keys_));
    pc_ += 2;
    waits_keyboard_ = false;
}

static std::uint8_t overflow_add(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& carry)
{
    static_assert(sizeof(int) > sizeof(std::uint8_t));
    const int v = (lhs + rhs);
    carry = ((v > 255) ? 1 : 0);
    return std::uint8_t(v);
}

static std::uint8_t overflow_sub(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& borrow)
{
    if (lhs >= rhs)
    {
        borrow = 0;
        return (lhs - rhs);
    }
    else // if (rhs > lhs)
    {
        borrow = 1;
        return (rhs - lhs);
    }
}

static std::uint8_t random_byte(std::random_device& rd)
{
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(0, int(std::uint8_t(-1)));
    return std::uint8_t(dist(gen));
}

///////////////////////////////////////////////////////////////////////////////
// Render loop.
#include <cstdlib>

constexpr uint8_t kKeymap[] =
{
    SDLK_x, SDLK_1, SDLK_2, SDLK_3,
    SDLK_q, SDLK_w, SDLK_e, SDLK_a,
    SDLK_s, SDLK_d, SDLK_z, SDLK_c,
    SDLK_4, SDLK_r, SDLK_f, SDLK_v,
};

static bool ReadAllFileAsBinary(const char* filepath
    , std::span<std::uint8_t> buffer)
{
#if (_MSC_VER)
    FILE* f = nullptr;
    const errno_t e = fopen_s(&f, filepath, "rb");
    (void)e;
#else
    FILE* const f = fopen(filepath, "rb");
#endif
    if (!f)
    {
        return false;
    }
    struct Close_
    {
        FILE* f_ = nullptr;
        ~Close_()
        {
            (void)fclose(f_);
        }
    } scope_close{f}; (void)scope_close;

    int status = fseek(f, 0, SEEK_END);
    if (status != 0)
    {
        return false;
    }
    const long fsize = ftell(f);
    if (fsize == -1L)
    {
        return false;
    }
    status = fseek(f, 0, SEEK_SET);
    if (status != 0)
    {
        return false;
    }
    const std::size_t size_to_read = static_cast<std::size_t>(fsize);
    if (size_to_read > buffer.size())
    {
        return false;
    }
    const std::size_t read = fread(&buffer[0], 1, size_to_read, f);
    if (read != size_to_read)
    {
        return false;
    }
    return true;
}

static void AbortOnSDLError(int code)
{
    if (code != 0)
    {
        assert(false && "SDL call failed");
        abort();
    }
}

static void AbortOnSDLError(const void* resource)
{
    if (!resource)
    {
        assert(false && "SDL call (resource) failed");
        abort();
    }
}

struct TickData
{
    Chip8& chip8_;
    // Leak all resources intentionally.
    SDL_Renderer* renderer_ = nullptr;
    SDL_Texture* picture_ = nullptr;
    std::uint32_t* pixels_ = nullptr;
    bool quit_ = false;
};

static void MainTick(void* data_ptr)
{
    TickData* data = static_cast<TickData*>(data_ptr);
    Chip8& chip8 = data->chip8_;
    SDL_Renderer* renderer = data->renderer_;

    SDL_Event e{};
    while (SDL_PollEvent(&e))
    {
        switch (e.type)
        {
        case SDL_QUIT:
            data->quit_ = true;
            break;
        case SDL_KEYDOWN:
            for (std::size_t i = 0; i < std::size(kKeymap); ++i)
            {
                if (e.key.keysym.sym == kKeymap[i])
                {
                    chip8.keys_[i] = true;
                    if (chip8.waits_keyboard_)
                    {
                        chip8.execute_cycle();
                    }
                    break;
                }
            }
            break;
        case SDL_KEYUP:
            for (std::size_t i = 0; i < std::size(kKeymap); ++i)
            {
                if (e.key.keysym.sym == kKeymap[i])
                {
                    chip8.keys_[i] = false;
                    break;
                }
            }
            if (e.key.keysym.sym == SDLK_ESCAPE)
            {
                data->quit_ = true;
            }
            break;
        }
    }

    // Our tick is called at 60 Hz frequency
    // most of the time (Vsync is ON).
    // Simulate CPU at (60 Hz * 10) = 600 Hz.
    // This is "unfair" since time between ticks
    // (execute_cycle()) is not uniform.
    for (int i = 0; i < 10; ++i)
    {
        if (chip8.waits_keyboard_)
        {
            break;
        }
        chip8.execute_cycle();
    }
    
    if (std::exchange(chip8.needs_redraw_, false))
    {
        for (int r = 0; r < kDisplayWidth; ++r)
        {
            for (int c = 0; c < kDisplayHeight; ++c)
            {
                const std::uint8_t on = chip8.display_memory_[r][c];
                const auto index = (r + c * kDisplayWidth);
                data->pixels_[index] = (on ? 0xff33ff66 : 0xff000000);
            }
        }

        AbortOnSDLError(SDL_UpdateTexture(data->picture_
            , nullptr
            , data->pixels_
            , kDisplayWidth * sizeof(std::uint32_t)));
    }

    AbortOnSDLError(SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff));
    AbortOnSDLError(SDL_RenderClear(renderer));
    AbortOnSDLError(SDL_RenderCopy(renderer, data->picture_, nullptr, nullptr));
    SDL_RenderPresent(renderer);
}

#if (__EMSCRIPTEN__)
#include <emscripten/emscripten.h>

static void MainLoop(TickData& data)
{
    emscripten_set_main_loop_arg(&MainTick
        , &data
        , -1  // use whatever FPS browser needs
        , 1); // simulate infinite loop. Don't destroy objects on stack (?)
}

int main(int, char**)
{

#else

#include <Windows.h>
#include <tchar.h>

static void MainLoop(TickData& data)
{
    while (!data.quit_)
    {
        MainTick(&data);
    }
}

int WINAPI _tWinMain(_In_ HINSTANCE, _In_opt_ HINSTANCE, _In_ LPTSTR, _In_ int)
{
    SDL_SetMainReady();
#endif
    // Initialize SDL. Ignore any errors and leak resources.
    AbortOnSDLError(SDL_Init(SDL_INIT_VIDEO));

    SDL_Window* window = SDL_CreateWindow(
        "Chip8 Emulator" // title
        , SDL_WINDOWPOS_CENTERED // x position
        , SDL_WINDOWPOS_CENTERED // y position
        , kDisplayWidth * 10
        , kDisplayHeight * 10
        , SDL_WINDOW_SHOWN | SDL_WINDOW_BORDERLESS);
    AbortOnSDLError(window);
    SDL_Renderer* renderer = SDL_CreateRenderer(
        window
        , -1 // first supporting renderer
        , SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    AbortOnSDLError(renderer);

    SDL_Texture* picture = SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        kDisplayWidth, kDisplayHeight);
    AbortOnSDLError(picture);

    std::uint32_t pixels[kDisplayWidth * kDisplayHeight]{};

    std::random_device rd;
    Chip8 chip8(rd);
    chip8.boot_up();

    const bool read = ReadAllFileAsBinary(R"(roms/WIPEOFF)"
        , {chip8.memory_ + chip8.pc_, (std::size(chip8.memory_) - chip8.pc_)});
    assert(read);

    TickData data{chip8, renderer, picture, pixels};
    MainLoop(data);

    return 0;
}
