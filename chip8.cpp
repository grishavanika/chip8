#include "chip8.hh"

#include <memory>
#include <string>

#include <cassert>
#include <cstdlib>

#include <SDL2/SDL.h>
#include <SDL2/SDL2_gfxPrimitives.h>
#include <SDL2/SDL_ttf.h>

#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace ra = ranges::actions;

struct KeyInfo
{
    const int code;
    const char* const title;
};

static constexpr KeyInfo kKeymap[] =
{
      {SDLK_x, "0 (X)"}, {SDLK_1, "1 (1)"}, {SDLK_2, "2 (2)"}, {SDLK_3, "3 (3)"}
    , {SDLK_q, "4 (Q)"}, {SDLK_w, "5 (W)"}, {SDLK_e, "6 (E)"}, {SDLK_a, "7 (A)"}
    , {SDLK_s, "8 (S)"}, {SDLK_d, "9 (D)"}, {SDLK_z, "A (Z)"}, {SDLK_c, "B (C)"}
    , {SDLK_4, "C (4)"}, {SDLK_r, "D (R)"}, {SDLK_f, "E (F)"}, {SDLK_v, "F (V)"}
};
static_assert(std::size(kKeymap) == 16);

static bool ReadAllFileAsBinary(std::string filepath
    , std::span<std::uint8_t> buffer)
{
#if (_MSC_VER)
    FILE* f = nullptr;
    const errno_t e = fopen_s(&f, filepath.c_str(), "rb");
    (void)e;
#else
    FILE* const f = fopen(filepath.c_str(), "rb");
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

static void CheckSDL(int code)
{
    if (code != 0)
    {
        assert(false && "SDL call failed");
        abort();
    }
}

static void CheckSDL(const void* resource)
{
    if (!resource)
    {
        assert(false && "SDL call (resource) failed");
        abort();
    }
}

static const char* kKnownROMs[] =
{
    "WIPEOFF",  "15PUZZLE", "BLINKY", "BLITZ",
    "BRIX",     "CONNECT4", "GUESS",  "HIDDEN",
    "INVADERS", "KALEID",   "MAZE",   "MERLIN",
    "MISSILE",  "PONG",     "PONG2",  "PUZZLE",
    "SYZYGY",   "TANK",     "TETRIS", "TICTAC",
    "UFO",      "VBRIX",    "VERS",   "IBM"
};

static std::unique_ptr<Chip8> LoadKnownROM(std::random_device& rd, std::size_t index)
{
    std::unique_ptr<Chip8> chip8(new Chip8{});
    chip8->rd_ = &rd;
    chip8->boot_up();

    const char* const path = kKnownROMs[index % std::size(kKnownROMs)];
    const bool ok = ReadAllFileAsBinary(std::string("assets/roms/") + path
        , {chip8->memory_ + chip8->pc_, (std::size(chip8->memory_) - chip8->pc_)});
    assert(ok);
    return chip8;
}

static constexpr int kRenderWidth = kDisplayWidth * 10;   // 640
static constexpr int kRenderHeight = kDisplayHeight * 10; // 320

static_assert((16 == std::size(kKeymap))
    && (16 == (sizeof(Keyboard::keys_) * CHAR_BIT))
    , "Keyboard rendering assumes 16 hex keys that form 4x4 grid");

static constexpr int kRenderKeySize = 50;
static constexpr int kRenderLineWidth = 2;
static constexpr int kRenderKeysWidth = kRenderKeySize * 4 + kRenderLineWidth * (4 + 1);
static constexpr int kRenderKeysHeight = 4 * kRenderKeySize + kRenderLineWidth;
static constexpr SDL_Color kKeyboardColor{0xfa, 0xd5, 0xd5, 0xff};
static constexpr SDL_Color kKeyPressColor{0x9b, 0x47, 0x7f, 0xff};

struct SDLTextureClose { void operator()(SDL_Texture* t) const { SDL_DestroyTexture(t); } };
using Texture = std::unique_ptr<SDL_Texture, SDLTextureClose>;

struct TickData
{
    std::random_device* rd_ = nullptr;
    std::unique_ptr<Chip8> chip8_;
    std::size_t rom_index_ = 0;
    bool quit_ = false;

    std::uint32_t* pixels_ = nullptr;

    // Leak all resources intentionally.
    SDL_Renderer* renderer_ = nullptr;
    TTF_Font* font_ = nullptr;

    Texture picture_;
    Texture keyboard_;
    Texture grid_;
    Texture key_titles_;
    Texture rom_title_;
    SDL_Rect rom_title_rect_{};
};

static Texture DrawTextToTexture(SDL_Renderer* renderer
    , TTF_Font* font
    , const char* text
    , SDL_Rect& size
    , SDL_Color color)
{
    SDL_Surface* surface = TTF_RenderUTF8_Blended(
        font, text, color);
    CheckSDL(surface);
    Texture texture(SDL_CreateTextureFromSurface(renderer, surface));
    CheckSDL(texture.get());

    size = {};
    size.w = surface->w;
    size.h = surface->h;
    SDL_FreeSurface(surface);

    return texture;
}

static Texture PrerenderKeyboardGrid(SDL_Renderer* renderer, SDL_Color color)
{
    Texture grid(SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_TARGET,
        kRenderKeysWidth, kRenderKeysHeight));
    CheckSDL(grid.get());

    CheckSDL(SDL_SetRenderTarget(renderer, grid.get()));
    CheckSDL(SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff));
    CheckSDL(SDL_RenderClear(renderer));
    CheckSDL(SDL_SetRenderDrawColor(renderer, 0xff, 0, 0, 0xff));

    // 4 keys per line (4 columns) = 5 vertical lines/separator.
    ranges::for_each(rv::iota(0, 4 + 1)
        , [&](int i)
    {
        // + 1 pixel because it seems SDL is off by one when
        // doing SDL_RenderCopy().
        thickLineRGBA(renderer
            , Sint16(i * kRenderKeySize + (i * kRenderLineWidth) + 1)
            , Sint16(0)
            , Sint16(i * kRenderKeySize + (i * kRenderLineWidth) + 1)
            , Sint16(kRenderKeysHeight)
            , Uint8(kRenderLineWidth)
            , color.r, color.g, color.b, color.a);

        thickLineRGBA(renderer
            , Sint16(0)
            , Sint16(i * kRenderKeySize + 1)
            , Sint16(kRenderKeysWidth)
            , Sint16(i * kRenderKeySize + 1)
            , Uint8(kRenderLineWidth)
            , color.r, color.g, color.b, color.a);
    });

    CheckSDL(SDL_SetRenderTarget(renderer, nullptr/*reset to default*/));
    return grid;
}

static Texture PrerenderKeyTitles(SDL_Renderer* renderer
    , TTF_Font* font, SDL_Color color)
{
    Texture texture(SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_TARGET,
        kRenderKeysWidth, kRenderKeysHeight));
    CheckSDL(texture.get());

    // BLEND with render clear Alpha = 0 is important there
    // so titles are properly mixed with other layers.
    CheckSDL(SDL_SetTextureBlendMode(texture.get(), SDL_BLENDMODE_BLEND));
    CheckSDL(SDL_SetRenderTarget(renderer, texture.get()));
    CheckSDL(SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0));
    CheckSDL(SDL_RenderClear(renderer));

    const auto indexes = rv::iota(std::uint8_t(0), std::uint8_t(4));
    auto pipeline = rv::cartesian_product(indexes, indexes)
        // #ranges: is there curry analog ? Maybe transform_curry()/tansform_unzip()
        // to take a tuple and make apply() on it ?
        | rv::transform([&](std::tuple<std::uint8_t, std::uint8_t> xy)
    {
        auto [x, y] = xy;
        const std::uint8_t key_id = std::uint8_t(x + y * 4);
        SDL_Rect size{};
        Texture text = DrawTextToTexture(renderer
            , font
            , kKeymap[key_id].title
            , size
            , color);
        SDL_Rect position = size;
        position.x = x * kRenderKeySize + ((x + 1) * kRenderLineWidth);
        position.y = y * kRenderKeySize + kRenderLineWidth;
        // Center text in a grid.
        assert((kRenderKeySize >= size.w) && (kRenderKeySize >= size.h));
        position.x += ((kRenderKeySize - size.w) / 2);
        position.y += ((kRenderKeySize - size.h) / 2);
        return std::make_tuple(std::move(text), position);
    });

    ranges::for_each(pipeline | rv::move, [&](auto&& data)
    {
        auto&& [text, position] = data;
        CheckSDL(SDL_RenderCopy(renderer
            , text.get(), nullptr, &position));
    });

    return texture;
}

static void RenderKeyboard(SDL_Renderer* renderer
    , Texture& texture
    , Texture& grid
    , Texture& titles
    , const Keyboard& chip8_keys)
{
    CheckSDL(SDL_SetRenderTarget(renderer, texture.get()));
    CheckSDL(SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff));
    CheckSDL(SDL_RenderClear(renderer));
    CheckSDL(SDL_SetRenderDrawColor(renderer, 0xff, 0, 0, 0xff));

    const SDL_Rect keyboard{0, kRenderHeight - kRenderKeysHeight, kRenderKeysWidth, kRenderKeysHeight};

    if (chip8_keys.keys_ == 0)
    {
        // No pressed keys. Just grid + titles.
        CheckSDL(SDL_RenderCopy(renderer, grid.get(), nullptr, &keyboard));
        CheckSDL(SDL_RenderCopy(renderer, titles.get(), nullptr, &keyboard));
        CheckSDL(SDL_SetRenderTarget(renderer, nullptr/*reset to default*/));
        return;
    }

    // Grid + "pressed keys" (dynamic) + titles.
    CheckSDL(SDL_RenderCopy(renderer, grid.get(), nullptr, &keyboard));

    CheckSDL(SDL_SetRenderDrawColor(renderer
        , kKeyPressColor.r, kKeyPressColor.g, kKeyPressColor.b, kKeyPressColor.a));

    const auto indexes = rv::iota(std::uint8_t(0), std::uint8_t(4));
    auto pipeline = rv::cartesian_product(indexes, indexes)
        | rv::filter([&](std::tuple<std::uint8_t, std::uint8_t> xy)
    {
        auto [x, y] = xy;
        const std::uint8_t key_id = std::uint8_t(x + y * 4);
        return chip8_keys.is_pressed(key_id);
    })
        | rv::transform([&](std::tuple<std::uint8_t, std::uint8_t> xy)
    {
        auto [x, y] = xy;
        const SDL_Rect rect =
        {
              x * kRenderKeySize + ((x + 1) * kRenderLineWidth)
            , y * kRenderKeySize + kRenderLineWidth + kRenderHeight - kRenderKeysHeight
            , kRenderKeySize
            , kRenderKeySize - kRenderLineWidth
        };
        return rect;
    });

    SDL_Rect pressed_keys[16]{};
    auto [in, out] = ranges::copy(pipeline, pressed_keys);
    const int count = int(std::distance(pressed_keys, out));

    assert(count > 0);
    CheckSDL(SDL_RenderFillRects(renderer, pressed_keys, count));
    CheckSDL(SDL_RenderCopy(renderer, titles.get(), nullptr, &keyboard));
    CheckSDL(SDL_SetRenderTarget(renderer, nullptr/*reset to default*/));
}

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
        {
            auto it = ranges::find(kKeymap, e.key.keysym.sym, &KeyInfo::code);
            ranges::for_each(rv::single(it)
                | rv::transform([](auto i) { return std::uint8_t(std::distance(std::begin(kKeymap), i)); })
                | rv::filter([](std::uint8_t index) { return index < std::size(kKeymap); })
                , [&](std::uint8_t index)
            {
                chip8->keyboard_.set_pressed(index);
                // Make sure we'll tick CPU.
                chip8->waits_keyboard_ = false;
            });
            break;
        }
        case SDL_KEYUP:
        {
            auto it = ranges::find(kKeymap, e.key.keysym.sym, &KeyInfo::code);
            ranges::for_each(rv::single(it)
                | rv::transform([](auto i) { return std::uint8_t(std::distance(std::begin(kKeymap), i)); })
                | rv::filter([](std::uint8_t index) { return index < std::size(kKeymap); })
                , [&](std::uint8_t index)
            {
                chip8->keyboard_.clear_pressed(index);
            });

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
    }

    if (old_rom_index != data->rom_index_)
    {
        data->chip8_ = LoadKnownROM(*data->rd_, data->rom_index_);
        data->rom_title_ = nullptr; // To be rendered later.
        chip8 = data->chip8_.get();
    }

    RenderKeyboard(renderer
        , data->keyboard_
        , data->grid_
        , data->key_titles_
        , chip8->keyboard_);

    // Our tick is called at 60 Hz frequency
    // most of the time (Vsync is ON).
    // Simulate CPU at (60 Hz * 9) = 540 Hz.
    // This is "unfair" since time between ticks
    // (execute_cycle()) is not uniform.
    //
    // Note: this makes "key pressed" to be true
    // for at least 9 cycles.
    ranges::for_each(rv::iota(0, 9) | rv::take_while([&](int)
        { return !chip8->waits_keyboard_; })
        , [&](int)
    {
        chip8->execute_cycle();
    });
    
    if (std::exchange(chip8->needs_redraw_, false))
    {
        auto pipeline = rv::cartesian_product(
              rv::iota(0, int(kDisplayHeight))
            , rv::iota(0, int(kDisplayWidth)))
            | rv::transform([&](std::tuple<int, int> cr)
        {
            auto [c, r] = cr;
            const std::uint8_t on = chip8->display_memory_[r][c];
            return std::uint32_t(on ? 0xff33ff66u : 0xff000000u);
        });
        ranges::copy(pipeline, data->pixels_);

        CheckSDL(SDL_UpdateTexture(data->picture_.get()
            , nullptr
            , data->pixels_
            , kDisplayWidth * sizeof(std::uint32_t)));
    }

    if (!data->rom_title_)
    {
        data->rom_title_ = DrawTextToTexture(renderer, data->font_
            , kKnownROMs[data->rom_index_ % std::size(kKnownROMs)]
            , data->rom_title_rect_, kKeyboardColor);
    }

    CheckSDL(SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0xff));
    CheckSDL(SDL_RenderClear(renderer));
    {
        SDL_Rect dest{0, 0, kRenderWidth, kRenderHeight};
        CheckSDL(SDL_RenderCopy(renderer, data->picture_.get(), nullptr, &dest));
    }
    {
        SDL_Rect dest{kRenderWidth, 0, kRenderKeysWidth, kRenderHeight};
        CheckSDL(SDL_RenderCopy(renderer, data->keyboard_.get(), nullptr, &dest));
    }
    {
        SDL_Rect dest{kRenderWidth, 0, data->rom_title_rect_.w, data->rom_title_rect_.h};
        CheckSDL(SDL_RenderCopy(renderer, data->rom_title_.get(), nullptr, &dest));
    }

    SDL_RenderPresent(renderer);
}

#if (__EMSCRIPTEN__)
#include <emscripten/emscripten.h>

static void MainLoop(TickData& data)
{
    emscripten_set_main_loop_arg(&MainTick
        , &data
        , 60
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
    CheckSDL(SDL_Init(SDL_INIT_VIDEO));
    CheckSDL(TTF_Init());

    SDL_Window* window = SDL_CreateWindow(
        "Chip8 Emulator" // title
        , SDL_WINDOWPOS_CENTERED // x position
        , SDL_WINDOWPOS_CENTERED // y position
        , kRenderWidth + kRenderKeysWidth
        , kRenderHeight
        , SDL_WINDOW_SHOWN | SDL_WINDOW_BORDERLESS);
    CheckSDL(window);
    SDL_Renderer* renderer = SDL_CreateRenderer(
        window
        , -1 // first supporting renderer
        , SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    CheckSDL(renderer);

    Texture picture(SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        kDisplayWidth, kDisplayHeight));
    CheckSDL(picture.get());

    Texture keyboard(SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_TARGET,
        kRenderKeysWidth, kRenderHeight));
    CheckSDL(keyboard.get());

    TTF_Font* font = TTF_OpenFont("assets/fonts/PlayfairDisplay-Bold.ttf", 20/*size*/);
    CheckSDL(font);

    std::uint32_t pixels[kDisplayWidth * kDisplayHeight]{};
    std::random_device rd;

    TickData data;
    data.rd_ = &rd;
    data.chip8_ = LoadKnownROM(rd, 0/*initial ROM*/);
    data.renderer_ = renderer;
    data.pixels_ = pixels;
    data.rom_title_ = nullptr;
    data.picture_ = std::move(picture);
    data.keyboard_ = std::move(keyboard);
    data.grid_ = PrerenderKeyboardGrid(renderer, kKeyboardColor);
    data.key_titles_ = PrerenderKeyTitles(renderer, font, kKeyboardColor);
    data.font_ = font;

    MainLoop(data);

    return 0;
}
