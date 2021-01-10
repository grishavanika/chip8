#pragma once
#include "code_edsl.h"

#include <random>
#include <chrono>
#include <span>

#include <cstddef>
#include <cmath>

#include <cassert>

// http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
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

// 60 * 1 tick = 1 second.
using Tick60Hz = std::chrono::duration<float, std::ratio<1, 60>>;

struct Chip8
{
    std::uint8_t memory_[4 * 1024]{};
    std::uint8_t V_[16]{};        // Vx, where x is [0; F]. VF should not be used.
    std::uint16_t stack_[16]{};   // Stack with `sp_`.
    std::uint16_t pc_{0};          // Program counter.
    std::uint16_t I_{0};           // Index register, 12 bits.
    Keyboard keyboard_{};
    std::uint8_t delay_timer_{0};  // Decremented at a rate of 60Hz.
    std::uint8_t sp_{0};           // Stack Pointer.
    std::uint8_t sound_timer_{0};  // Only one tone.
    std::uint8_t display_memory_[kDisplayWidth][kDisplayHeight]{};

    std::default_random_engine* random_{nullptr};
    bool needs_redraw_{false};         // For "optimizations".
    bool waits_keyboard_{false};
    std::chrono::high_resolution_clock::time_point last_tick_{};

    Chip8() = default;
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
static std::uint8_t random_byte(std::default_random_engine& random);

void Chip8::execute_cycle()
{
    const auto now = std::chrono::high_resolution_clock::now();
    const float elapsed_ticks = std::chrono::duration_cast<Tick60Hz>(now - last_tick_).count();
    if (elapsed_ticks >= 1.f)
    {
        last_tick_ = now;
    }
    delay_timer_ -= std::uint8_t(std::min<unsigned>(delay_timer_, unsigned(elapsed_ticks)));
    sound_timer_ -= std::uint8_t(std::min<unsigned>(sound_timer_, unsigned(elapsed_ticks)));

    const std::uint16_t opcode = std::uint16_t(
        (memory_[pc_] << 8) | (memory_[pc_ + 1]));
    pc_ += 2;
    execute_opcode(opcode);
}

void Chip8::execute_opcode(std::uint16_t opcode)
{
    using namespace edsl;
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
            cpu.V_[x] = random_byte(*cpu.random_) & kk; })
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
    last_tick_ = std::chrono::high_resolution_clock::now();
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
        // Mostly a hack: we "finished" waiting.
        // If code/cpu tries to wait for any key
        // again "too fast" (before our emulation
        // resets state) we should not report that
        // same key (any key) ready.
        keyboard_.keys_ = 0;
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

static std::uint8_t random_byte(std::default_random_engine& random)
{
    std::uniform_int_distribution<> dist(0, int(std::uint8_t(-1)));
    return std::uint8_t(dist(random));
}
