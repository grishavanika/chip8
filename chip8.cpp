#include <algorithm>
#include <numeric>
#include <memory>
#include <string>
#include <random>
#include <span>

#include <cstddef>
#include <cmath>

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

struct Keyboard
{
    std::uint16_t keys_ = 0; // 0-F (16) keys.

    bool is_pressed(std::uint8_t index) const;
    bool is_any_pressed(std::uint8_t& index) const;
    void set_pressed(std::uint8_t index);
    void clear_pressed(std::uint8_t index);
};

struct Chip8
{
    std::uint8_t memory_[4 * 1024];
    std::uint8_t V_[16];        // Vx, where x is [0; F]. VF should not be used.
    std::uint16_t stack_[16];   // Stack with `sp_`.
    std::uint16_t pc_;          // Program counter.
    std::uint16_t I_;           // Index register, 12 bits.
    Keyboard keyboard_;
    std::uint8_t delay_timer_;  // Decremented at a rate of 60Hz.
    std::uint8_t sp_;           // Stack Pointer.
    std::uint8_t sound_timer_;  // Only one tone.
    std::uint8_t display_memory_[kDisplayWidth][kDisplayHeight];

    std::random_device* rd_;
    bool needs_redraw_;         // For "optimizations".
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
        , keyboard_()
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
    bool try_wait_any_key(std::uint8_t vindex);
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
        , []() { assert(false && "Unknown opcode."); }
        , code(0x0, 0x0, 0xE, 0x0, [](Chip8& cpu)
        { /*CLS*/
            cpu.clear_display(); })
        , code(0x0, 0x0, 0xE, 0xE, [](Chip8& cpu)
        { /*RET*/
            assert(cpu.sp_ > 0);
            --cpu.sp_;
            assert(cpu.sp_ < std::size(cpu.stack_));
            cpu.pc_ = cpu.stack_[cpu.sp_]; })
        , code(0x0, _, _, _, []()
        { /* SYS addr. This instruction is only used on the old computers
            on which Chip-8 was originally implemented.
            It is ignored by modern interpreters. */ })
        , code(0x1, _n, _n, _n, [](Chip8& cpu, std::uint16_t nnn)
        { /* JP addr. */
            cpu.pc_ = nnn; })
        , code(0x2, _n, _n, _n, [](Chip8& cpu, std::uint16_t nnn)
        { /* CALL addr. */
            assert(cpu.sp_ < std::size(cpu.stack_));
            cpu.stack_[cpu.sp_] = cpu.pc_;
            ++cpu.sp_;
            cpu.pc_ = nnn; })
        , code(0x3, _x, _k, _k, [](Chip8& cpu, std::uint8_t x, std::uint8_t kk)
        { /* SE Vx, byte. */
            cpu.pc_ += ((cpu.V_[x] == kk) ? 2 : 0); })
        , code(0x4, _x, _k, _k, [](Chip8& cpu, std::uint8_t x, std::uint8_t kk)
        { /* SNE Vx, byte. */
            cpu.pc_ += ((cpu.V_[x] != kk) ? 2 : 0); })
        , code(0x5, _x, _y, 0x0, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* SE Vx, Vy. */
            cpu.pc_ += ((cpu.V_[x] == cpu.V_[y]) ? 2 : 0); })
        , code(0x6, _x, _k, _k, [](Chip8& cpu, std::uint8_t x, std::uint8_t kk)
        { /* LD Vx, byte. */
            cpu.V_[x] = kk; })
        , code(0x7, _x, _k, _k, [](Chip8& cpu, std::uint8_t x, std::uint8_t kk)
        { /* ADD Vx, byte. */
            cpu.V_[x] += kk; })
        , code(0x8, _x, _y, 0x0, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* LD Vx, Vy. */
            cpu.V_[x] = cpu.V_[y]; })
        , code(0x8, _x, _y, 0x1, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* OR Vx, Vy. */
            cpu.V_[x] = cpu.V_[x] | cpu.V_[y]; })
        , code(0x8, _x, _y, 0x2, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* AND Vx, Vy. */
            cpu.V_[x] = cpu.V_[x] & cpu.V_[y]; })
        , code(0x8, _x, _y, 0x3, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* XOR Vx, Vy. */
            cpu.V_[x] = cpu.V_[x] ^ cpu.V_[y]; })
        , code(0x8, _x, _y, 0x4, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* ADD Vx, Vy. */
            cpu.V_[x] = overflow_add(cpu.V_[x], cpu.V_[y], cpu.V_[0xF]); })
        , code(0x8, _x, _y, 0x5, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* SUB Vx, Vy. */
            cpu.V_[x] = overflow_sub(cpu.V_[x], cpu.V_[y], cpu.V_[0xF]); })
        , code(0x8, _x, _, 0x6, [](Chip8& cpu, std::uint8_t x)
        { /* SHR Vx {, Vy}. */
            cpu.V_[0xF] = cpu.V_[x] & 0x1;
            cpu.V_[x] >>= 1; })
        , code(0x8, _x, _y, 0x7, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* SUBN Vx, Vy. */
            cpu.V_[x] = overflow_sub(cpu.V_[y], cpu.V_[x], cpu.V_[0xF]); })
        , code(0x8, _x, _, 0xE, [](Chip8& cpu, std::uint8_t x)
        { /* SHL Vx {, Vy}. */
            cpu.V_[0xF] = cpu.V_[x] & 0x80;
            cpu.V_[x] <<= 1; })
        , code(0x9, _x, _y, 0x0, [](Chip8& cpu, std::uint8_t x, std::uint8_t y)
        { /* SNE Vx, Vy. */
            cpu.pc_ += ((cpu.V_[x] != cpu.V_[y]) ? 2 : 0); })
        , code(0xA, _n, _n, _n, [](Chip8& cpu, std::uint16_t nnn)
        { /* LD I, addr. */
            cpu.I_ = nnn; })
        , code(0xB, _n, _n, _n, [](Chip8& cpu, std::uint16_t nnn)
        { /* JP V0, addr. */
            cpu.pc_ = std::uint16_t(nnn + cpu.V_[0]); })
        , code(0xC, _x, _k, _k, [](Chip8& cpu, std::uint8_t x, std::uint8_t kk)
        { /* RND Vx, byte. */
            cpu.V_[x] = random_byte(*cpu.rd_) & kk; })
        , code(0xD, _x, _y, _n, [](Chip8& cpu, std::uint8_t x, std::uint8_t y, std::uint8_t n)
        { /* DRW Vx, Vy, nibble. */
            const bool off = cpu.draw(cpu.V_[x], cpu.V_[y]
                , {cpu.memory_ + cpu.I_, n});
            cpu.V_[0xF] = std::uint8_t(off);
            cpu.needs_redraw_ = true; })
        , code(0xE, _x, 0x9, 0xE, [](Chip8& cpu, std::uint8_t x)
        { /* SKP Vx. */
            const bool on = cpu.keyboard_.is_pressed(cpu.V_[x]);
            cpu.pc_ += (on ? 2 : 0); })
        , code(0xE, _x, 0xA, 0x1, [](Chip8& cpu, std::uint8_t x)
        { /* SKNP Vx. */
            const bool off = !cpu.keyboard_.is_pressed(cpu.V_[x]);
            cpu.pc_ += (off ? 2 : 0); })
        , code(0xF, _x, 0x0, 0x7, [](Chip8& cpu, std::uint8_t x)
        { /* LD Vx, DT. */
            cpu.V_[x] = cpu.delay_timer_; })
        , code(0xF, _x, 0x0, 0xA, [](Chip8& cpu, std::uint8_t x)
        { /* LD Vx, K. */
            cpu.waits_keyboard_ = cpu.try_wait_any_key(x); })
        , code(0xF, _x, 0x1, 0x5, [](Chip8& cpu, std::uint8_t x)
        { /* LD DT, Vx. */
            cpu.delay_timer_ = cpu.V_[x]; })
        , code(0xF, _x, 0x1, 0x8, [](Chip8& cpu, std::uint8_t x)
        { /* LD ST, Vx. */
            cpu.sound_timer_ = cpu.V_[x]; })
        , code(0xF, _x, 0x1, 0xE, [](Chip8& cpu, std::uint8_t x)
        { /* ADD I, Vx. */
            cpu.I_ += cpu.V_[x]; })
        , code(0xF, _x, 0x2, 0x9, [](Chip8& cpu, std::uint8_t x)
        { /* LD F, Vx. */
            const auto sprite_bytes = (std::size(kHexDigitsSprites) / 16);
            cpu.I_ = std::uint16_t(cpu.V_[x] * sprite_bytes); })
        , code(0xF, _x, 0x3, 0x3, [](Chip8& cpu, std::uint8_t x)
        { /* LD B, Vx. */
            std::uint8_t* ptr = (cpu.memory_ + cpu.I_);
            ptr[0] = (cpu.V_[x] / 100);
            ptr[1] = (cpu.V_[x] / 10) % 10;
            ptr[2] = (cpu.V_[x] % 100) % 10; })
        , code(0xF, _x, 0x5, 0x5, [](Chip8& cpu, std::uint8_t x)
        { /* LD [I], Vx. */
            std::copy(cpu.V_, cpu.V_ + x + 1
                , cpu.memory_ + cpu.I_); })
        , code(0xF, _x, 0x6, 0x5, [](Chip8& cpu, std::uint8_t x)
        { /* LD Vx, [I]. */
            std::copy(cpu.memory_ + cpu.I_, cpu.memory_ + cpu.I_ + x + 1
                , cpu.V_); })
        );

    assert(pc_ < std::size(memory_));
}

bool Keyboard::is_pressed(std::uint8_t index) const
{
    assert(index < 16);
    return bool((keys_ >> index) & 0x1u);
}

void Keyboard::set_pressed(std::uint8_t index)
{
    assert(index < 16);
    keys_ |= std::uint16_t(0x1u << index);
}

void Keyboard::clear_pressed(std::uint8_t index)
{
    assert(index < 16);
    keys_ &= std::uint16_t(~(0x1u << index));
}

bool Keyboard::is_any_pressed(std::uint8_t& index) const
{
    if (keys_ != 0)
    {
        // rightmost set bit.
        index = std::uint8_t(std::log2(keys_ & -keys_));
        return true;
    }
    return false;
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
        return std::uint8_t((value >> (8 - (index + 1))) & 0x01u);
    };

    struct DrawInput
    {
        const std::uint8_t new_pixel;
        const std::uint8_t x;
        const std::uint8_t y;
    };

    return outer_index_product(std::uint8_t(sprite.size()), 8, false
        , [this](bool already_off, DrawInput input)
    {
        std::uint8_t& now = display_memory_[input.x][input.y];
        std::uint8_t old = now;
        now ^= input.new_pixel;
        return (already_off || (!now && old));
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

bool Chip8::try_wait_any_key(std::uint8_t vindex)
{
    if (keyboard_.is_any_pressed(V_[vindex]))
    {
        return false;
    }

    pc_ -= 2; // busy wait.
    return true;
}

static std::uint8_t overflow_add(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& carry)
{
    static_assert(sizeof(int) > sizeof(std::uint8_t));
    const int v = (lhs + rhs);
    carry = ((v > 0xff) ? 1 : 0);
    return std::uint8_t(v);
}

static std::uint8_t overflow_sub(std::uint8_t lhs, std::uint8_t rhs
    , std::uint8_t& no_borrow)
{
    no_borrow = std::uint8_t(lhs >= rhs);
    return std::uint8_t(lhs - rhs);
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
static_assert(std::size(kKeymap) == 16);

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

const char* kKnownROMs[] = {
    "WIPEOFF", "15PUZZLE", "BLINKY", "BLITZ", "BRIX", "CONNECT4", "GUESS", "HIDDEN",
    "INVADERS", "KALEID", "MAZE", "MERLIN", "MISSILE", "PONG", "PONG2", "PUZZLE",
    "SYZYGY", "TANK", "TETRIS", "TICTAC", "UFO", "VBRIX", "VERS", "IBM"};

static std::unique_ptr<Chip8> LoadKnownROM(std::random_device& rd, std::size_t index)
{
    auto chip8 = std::make_unique<Chip8>(rd);
    chip8->boot_up();

    const char* const name = kKnownROMs[index % std::size(kKnownROMs)];
    const bool ok = ReadAllFileAsBinary((std::string("roms/") + name).c_str()
        , {chip8->memory_ + chip8->pc_, (std::size(chip8->memory_) - chip8->pc_)});
    assert(ok);
    return chip8;
}

struct TickData
{
    std::random_device* rd_ = nullptr;
    std::unique_ptr<Chip8> chip8_;
    std::size_t rom_index_ = 0;
    // Leak all resources intentionally.
    SDL_Renderer* renderer_ = nullptr;
    SDL_Texture* picture_ = nullptr;
    std::uint32_t* pixels_ = nullptr;
    bool quit_ = false;
};

static void MainTick(void* data_ptr)
{
    TickData* data = static_cast<TickData*>(data_ptr);
    SDL_Renderer* renderer = data->renderer_;
    Chip8* chip8 = data->chip8_.get();
    assert(chip8);
    std::size_t old_rom_index = data->rom_index_;

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
                    chip8->keyboard_.set_pressed(std::uint8_t(i));
                    if (chip8->waits_keyboard_)
                    {
                        chip8->execute_cycle();
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
                    chip8->keyboard_.clear_pressed(std::uint8_t(i));
                    break;
                }
            }
            if (e.key.keysym.sym == SDLK_ESCAPE)
            {
                data->quit_ = true;
            }
            if (e.key.keysym.sym == SDLK_RIGHT)
            {
                ++data->rom_index_;
            }
            if (e.key.keysym.sym == SDLK_LEFT)
            {
                --data->rom_index_;
            }
            break;
        }
    }

    if (old_rom_index != data->rom_index_)
    {
        data->chip8_ = LoadKnownROM(*data->rd_, data->rom_index_);
        chip8 = data->chip8_.get();
    }

    // Our tick is called at 60 Hz frequency
    // most of the time (Vsync is ON).
    // Simulate CPU at (60 Hz * 10) = 600 Hz.
    // This is "unfair" since time between ticks
    // (execute_cycle()) is not uniform.
    for (int i = 0; i < 10; ++i)
    {
        if (chip8->waits_keyboard_)
        {
            break;
        }
        chip8->execute_cycle();
    }
    
    if (std::exchange(chip8->needs_redraw_, false))
    {
        for (int r = 0; r < kDisplayWidth; ++r)
        {
            for (int c = 0; c < kDisplayHeight; ++c)
            {
                const std::uint8_t on = chip8->display_memory_[r][c];
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
    auto chip8 = LoadKnownROM(rd, 0);
    TickData data{&rd, std::move(chip8), 0, renderer, picture, pixels};
    MainLoop(data);

    return 0;
}
