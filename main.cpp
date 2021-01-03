#include <concepts>
#include <variant>
#include <algorithm>
#include <numeric>
#include <random>
#include <span>

#include <cstdint>
#include <cstddef>

#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include <cassert>

#include <SDL2/SDL.h>

static_assert(CHAR_BIT == 8);

struct byte_placeholder {};
constexpr byte_placeholder _;
constexpr byte_placeholder _n; // Same as _.
constexpr byte_placeholder _x;
constexpr byte_placeholder _y;
constexpr byte_placeholder _k;

struct Match
{
    using Byte = std::variant<byte_placeholder, std::uint8_t>;
    Byte opcode_[4];

    static constexpr Byte make_byte(byte_placeholder)
    {
        return Byte(_);
    }

    static constexpr Byte make_byte(std::integral auto v)
    {
        return Byte(std::uint8_t(v));
    }

    bool check(std::uint16_t opcode) const
    {
        const int bytes[] =
        {
            (opcode & 0xF000u) >> 12u,
            (opcode & 0x0F00u) >>  8u,
            (opcode & 0x00F0u) >>  4u,
            (opcode & 0x000Fu) >>  0u,
        };

        return std::inner_product(std::cbegin(opcode_), std::end(opcode_)
            , std::cbegin(bytes)
            , true
            , [](bool prev, bool now) { return (prev && now); }
            , [](Byte b, int v)
        {
            if (std::get_if<byte_placeholder>(&b))
            {
                // Any value satisfies.
                return true;
            }
            else if (const std::uint8_t* to_match = std::get_if<std::uint8_t>(&b))
            {
                return (*to_match == std::uint8_t(v));
            }
            return false;
        });
    }
};

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

template<typename F>
struct OpCodeOperation
{
    F op_;
    Match match_;

    bool operator()(std::uint16_t v) const
    {
        if (match_.check(v))
        {
            op_();
            return true;
        }
        return false;
    }
};

template<typename F>
OpCodeOperation(F, Match) -> OpCodeOperation<F>;

template<typename T>
concept AnyByte = std::is_integral_v<T> || std::is_same_v<T, byte_placeholder>;

template<typename F, typename R = void>
concept FunctionNoArg = std::invocable<F>
    && std::is_same_v<R, std::invoke_result_t<F>>;

template<typename F, typename P, typename R = void>
concept FunctionOneArg = std::invocable<F, P>
    && std::is_same_v<R, std::invoke_result_t<F, P>>;

constexpr auto code(AnyByte auto b1, AnyByte auto b2
    , AnyByte auto b3, AnyByte auto b4
    , FunctionNoArg auto f)
{
    return OpCodeOperation(std::move(f)
        , Match({Match::make_byte(b1), Match::make_byte(b2),
                 Match::make_byte(b3), Match::make_byte(b4)}));
}

template<typename... F>
void match_opcode(std::uint16_t opcode, FunctionNoArg auto catch_all
    // error C3546: '...': there are no parameter packs available to expand
    //, FunctionOneArg<std::uint16_t/*parameter*/, bool/*return type*/> auto... ops)
    , F... ops)
{
    const bool handled = (ops(opcode) || ...);
    if (!handled)
    {
        catch_all();
    }
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

// ROMS.
// https://github.com/ColinEberhardt/wasm-rust-chip8/tree/master/web/roms

static constexpr std::uint8_t kDisplayWidth = 64;
static constexpr std::uint8_t kDisplayHeight = 32;

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

// http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
// https://github.com/JamesGriffin/CHIP-8-Emulator
// https://blog.scottlogic.com/2017/12/13/chip8-emulator-webassembly-rust.html
// 
struct Chip8
{
    std::uint8_t memory_[4 * 1024];
    std::uint8_t registers_[16];   // Vx, where x is [0; F]. VF should not be used.
    std::uint16_t stack_[16];      // Stack with `stack_pointer_`.
    std::uint16_t pc_;             // Program counter (PC).
    std::uint16_t I_;              // I, 12 bits.
    std::uint8_t delay_timer_;     // Decremented at a rate of 60Hz.
    std::uint8_t stack_pointer_;   // SP.
    std::uint8_t sound_timer_;     // Only one tone.
    std::uint8_t display_memory_[kDisplayWidth][kDisplayHeight];
    bool keys_[16];

    std::random_device* rd_;
    bool needs_redraw_;
    bool waits_keyboard_;

    explicit Chip8(std::random_device& rd)
        : memory_()
        , registers_()
        , stack_()
        , pc_(0)
        , I_(0)
        , delay_timer_(0)
        , stack_pointer_(0)
        , sound_timer_(0)
        , display_memory_()
        , keys_()
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

    void start_with_rom(const void* data, std::size_t size);
    bool draw(std::uint8_t x, std::uint8_t y
        , std::span<const std::uint8_t> sprite);
    void clear_display();
    void wait_any_key(std::uint8_t vindex);
    void execute_cycle();
    void execute_opcode(std::uint16_t opcode);
};

void Chip8::execute_cycle()
{
    const std::uint16_t opcode = std::uint16_t(
        (memory_[pc_] << 8) | (memory_[pc_ + 1]));
    execute_opcode(opcode);
}

void Chip8::execute_opcode(std::uint16_t opcode)
{
    const std::uint16_t nnn = std::uint16_t(opcode & 0x0FFFu);
    const std::uint8_t x = std::uint8_t((opcode & 0x0F00u) >> 8u);
    const std::uint8_t y = std::uint8_t((opcode & 0x00F0u) >> 4u);
    assert(x < std::size(registers_));
    assert(y < std::size(registers_));
    std::uint8_t& Vx = registers_[x];
    std::uint8_t& Vy = registers_[y];
    std::uint8_t& VF = registers_[0xF];
    // Not a mistake, we get 2 bytes and cast them to single byte.
    const std::uint8_t kk = std::uint8_t(opcode & 0x00FFu);
    std::uint8_t n = std::uint8_t(opcode & 0x000Fu);

    pc_ += 2;

    match_opcode(opcode
        , []() { assert(false && "Unknown opcode."); }
        , code(0x0, 0x0, 0xE, 0x0, [&]()
        { /*CLS*/
            clear_display(); })
        , code(0x0, 0x0, 0xE, 0xE, [&]()
        { /*RET*/
            assert(stack_pointer_ > 0);
            --stack_pointer_;
            assert(stack_pointer_ < std::size(stack_));
            pc_ = stack_[stack_pointer_]; })
        , code(0x0, _n, _n, _n, []()
        { /* SYS addr. This instruction is only used on the old computers
            on which Chip-8 was originally implemented.
            It is ignored by modern interpreters. */ })
        , code(0x1, _n, _n, _n, [&]()
        { /* JP addr. */
            pc_ = nnn; })
        , code(0x2, _n, _n, _n, [&]()
        { /* CALL addr. */
            assert(stack_pointer_ < std::size(stack_));
            stack_[stack_pointer_] = pc_;
            ++stack_pointer_;
            pc_ = nnn; })
        , code(0x3, _x, _k, _k, [&]()
        { /* SE Vx, byte. */
            pc_ += ((Vx == kk) ? 2 : 0); })
        , code(0x4, _x, _k, _k, [&]()
        { /* SNE Vx, byte. */
            pc_ += ((Vx != kk) ? 2 : 0); })
        , code(0x5, _x, _y, 0x0, [&]()
        { /* SE Vx, Vy. */
            pc_ += ((Vx == Vy) ? 2 : 0); })
        , code(0x6, _x, _k, _k, [&]()
        { /* LD Vx, byte. */
            Vx = kk; })
        , code(0x7, _x, _k, _k, [&]()
        { /* ADD Vx, byte. */
            Vx += kk; })
        , code(0x8, _x, _y, 0x0, [&]()
        { /* LD Vx, Vy. */
            Vx = Vy; })
        , code(0x8, _x, _y, 0x1, [&]()
        { /* OR Vx, Vy. */
            Vx = Vx | Vy; })
        , code(0x8, _x, _y, 0x2, [&]()
        { /* AND Vx, Vy. */
            Vx = Vx & Vy; })
        , code(0x8, _x, _y, 0x3, [&]()
        { /* XOR Vx, Vy. */
            Vx = Vx ^ Vy; })
        , code(0x8, _x, _y, 0x4, [&]()
        { /* ADD Vx, Vy. */
            Vx = overflow_add(Vx, Vy, VF); })
        , code(0x8, _x, _y, 0x5, [&]()
        { /* SUB Vx, Vy. */
            Vx = overflow_sub(Vx, Vy, VF);
            VF = ((VF == 0) ? 1 : 0); })
        , code(0x8, _x, _y, 0x6, [&]()
        { /* SHR Vx {, Vy}. */
            VF = Vx & 0x1;
            Vx >>= 1; })
        , code(0x8, _x, _y, 0x7, [&]()
        { /* SUBN Vx, Vy. */
            Vx = overflow_sub(Vy, Vx, VF);
            VF = ((VF == 0) ? 1 : 0); })
        , code(0x8, _x, _y, 0xE, [&]()
        { /* SHL Vx {, Vy}. */
            VF = Vx & 0x80;
            Vx <<= 1; })
        , code(0x9, _x, _y, 0x0, [&]()
        { /* SNE Vx, Vy. */
            pc_ += ((Vx != Vy) ? 2 : 0); })
        , code(0xA, _n, _n, _n, [&]()
        { /* LD I, addr. */
            I_ = nnn; })
        , code(0xB, _n, _n, _n, [&]()
        { /* JP V0, addr. */
            pc_ = std::uint16_t(nnn + registers_[0]); })
        , code(0xC, _x, _k, _k, [&]()
        { /* RND Vx, byte. */
            Vx = random_byte(*rd_) & kk; })
        , code(0xD, _x, _y, _n, [&]()
        { /* DRW Vx, Vy, nibble. */
            const bool collision = draw(Vx, Vy, {memory_ + I_, n});
            needs_redraw_ = true;
            VF = (collision ? 1 : 0); })
        , code(0xE, _x, 0x9, 0xE, [&]()
        { /* SKP Vx. */
            pc_ += (keys_[Vx] ? 2 : 0); })
        , code(0xE, _x, 0xA, 0x1, [&]()
        { /* SKNP Vx. */
            pc_ += (!keys_[Vx] ? 2 : 0); })
        , code(0xF, _x, 0x0, 0x7, [&]()
        { /* LD Vx, DT. */
            Vx = delay_timer_; })
        , code(0xF, _x, 0x0, 0xA, [&]()
        { /* LD Vx, K. */
            wait_any_key(x); })
        , code(0xF, _x, 0x1, 0x5, [&]()
        { /* LD DT, Vx. */
            delay_timer_ = Vx; })
        , code(0xF, _x, 0x1, 0x8, [&]()
        { /* LD ST, Vx. */
            sound_timer_ = Vx; })
        , code(0xF, _x, 0x1, 0xE, [&]()
        { /* ADD I, Vx. */
            I_ += Vx; })
        , code(0xF, _x, 0x2, 0x9, [&]()
        { /* LD F, Vx. */
            const auto sprite_bytes = (std::size(kHexDigitsSprites) / 16);
            I_ = std::uint16_t(Vx * sprite_bytes); })
        , code(0xF, _x, 0x3, 0x3, [&]()
        { /* LD B, Vx. */
            std::uint8_t* ptr = (memory_ + I_);
            ptr[0] = (Vx / 100);
            ptr[1] = (Vx / 10) % 10;
            ptr[2] = (Vx % 100) % 10; })
        , code(0xF, _x, 0x5, 0x5, [&]()
        { /* LD [I], Vx. */
            std::copy(registers_, registers_ + x + 1
                , memory_ + I_); })
        , code(0xF, _x, 0x6, 0x5, [&]()
        { /* LD Vx, [I]. */
            std::copy(memory_ + I_, memory_ + I_ + x + 1
                , registers_); })
        );

    assert(pc_ < std::size(memory_));

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
    registers_[vindex] = std::uint8_t(it - std::cbegin(keys_));
    pc_ += 2;
    waits_keyboard_ = false;
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

void Chip8::start_with_rom(const void* data, std::size_t size)
{
    pc_ = 0x200;
    assert((std::size(memory_) - pc_) >= size);
    const std::uint8_t* begin = static_cast<const std::uint8_t*>(data);
    std::copy(begin, begin + size, memory_ + pc_);
}

//-----------------------------------------------------------------------------
#include <chrono>
#include <thread>
#include <cstdlib>

struct FileBuffer
{
    void* data_ = nullptr;
    std::size_t size_ = 0;

    FileBuffer() noexcept = default;
    FileBuffer(const FileBuffer&) = delete;
    FileBuffer& operator=(const FileBuffer&) = delete;
    FileBuffer& operator=(FileBuffer&&) = delete;

    FileBuffer(FileBuffer&& rhs) noexcept;
    ~FileBuffer() noexcept;
};

FileBuffer ReadAllFileAsBinary(const char* filepath);

FileBuffer::FileBuffer(FileBuffer&& rhs) noexcept
    : data_(std::exchange(rhs.data_, nullptr))
    , size_(std::exchange(rhs.size_, 0))
{
}

FileBuffer::~FileBuffer() noexcept
{
    free(data_);
    data_ = nullptr;
    size_ = 0;
}

FileBuffer ReadAllFileAsBinary(const char* filepath)
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
        return {};
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
        return {};
    }
    const long fsize = ftell(f);
    if (fsize == -1L)
    {
        return {};
    }
    status = fseek(f, 0, SEEK_SET);
    if (status != 0)
    {
        return {};
    }
    const std::size_t size_to_read = static_cast<std::size_t>(fsize);
    FileBuffer buffer;
    buffer.data_ = malloc(size_to_read);
    buffer.size_ = size_to_read;
    if (!buffer.data_)
    {
        return {};
    }
    const std::size_t read = fread(buffer.data_, 1, size_to_read, f);
    if (read != size_to_read)
    {
        return {};
    }
    return buffer;
}

void AbortOnSDLError(int code)
{
    if (code != 0)
    {
        assert(false && "SDL call failed");
        abort();
    }
}

void AbortOnSDLError(const void* resource)
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
    SDL_Renderer* renderer_ = nullptr;
    SDL_Texture* picture_ = nullptr;
    std::uint32_t* pixels_ = nullptr;
    bool quit_ = false;
};

constexpr uint8_t kKeymap[] =
{
    SDLK_x, SDLK_1, SDLK_2, SDLK_3,
    SDLK_q, SDLK_w, SDLK_e, SDLK_a,
    SDLK_s, SDLK_d, SDLK_z, SDLK_c,
    SDLK_4, SDLK_r, SDLK_f, SDLK_v,
};

void MainTick(void* data_ptr)
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

void MainLoop(TickData& data)
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

void MainLoop(TickData& data)
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
        , 640
        , 320
        , SDL_WINDOW_SHOWN | SDL_WINDOW_BORDERLESS);
    AbortOnSDLError(window);
    SDL_Renderer* renderer = SDL_CreateRenderer(
        window
        , -1 // first supporting renderer
        , SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    AbortOnSDLError(renderer);

    std::random_device rd;
    Chip8 chip8(rd);

    SDL_Texture* picture = SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        kDisplayWidth, kDisplayHeight);
    AbortOnSDLError(picture);

    std::uint32_t pixels[kDisplayWidth * kDisplayHeight]{};

    //FileBuffer file = ReadAllFileAsBinary(R"(D:\Downloads\TICTAC)");
    FileBuffer file = ReadAllFileAsBinary(R"(D:\Downloads\WIPEOFF)");
    //FileBuffer file = ReadAllFileAsBinary(R"(D:\Downloads\INVADERS)");
    
    assert(!!file.data_);

    chip8.start_with_rom(file.data_, file.size_);
    chip8.needs_redraw_ = true;

    TickData data{chip8, renderer, picture, pixels};
    MainLoop(data);

    return 0;
}
