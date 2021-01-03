#include <variant>
#include <tuple>
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

#if (__EMSCRIPTEN__)
# define X_HAS_CONCEPTS() 0
#else
# define X_HAS_CONCEPTS() 1
#endif

#if (X_HAS_CONCEPTS())
#  include <concepts>
#endif

#include <SDL2/SDL.h>

static_assert(CHAR_BIT == 8);

template<int Tag_>
struct byte_placeholder : std::integral_constant<int, Tag_> {};

constexpr byte_placeholder<-1> _;
constexpr byte_placeholder<0> _n;
constexpr byte_placeholder<1> _x;
constexpr byte_placeholder<2> _y;
constexpr byte_placeholder<3> _k;

template<typename P>
struct is_placeholder : std::false_type {};

template<int I>
struct is_placeholder<byte_placeholder<I>> : std::true_type {};

template<typename P>
constexpr bool is_placeholder_v = is_placeholder<P>::value;

template<typename BinOp, typename Init, typename Tuple>
struct reduce;

template<typename BinOp, typename Init>
struct reduce<BinOp, Init, std::tuple<>> { using type = Init; };

template<typename BinOp, typename Init, typename A, typename... Args>
struct reduce<BinOp, Init, std::tuple<A, Args...>>
    : reduce<BinOp
        , decltype(std::declval<BinOp>()(std::declval<Init>(), std::declval<A>()))
        , std::tuple<Args...>> {};

template<typename BinOp, typename Init, typename Tuple>
using reduce_t = typename reduce<BinOp, Init, Tuple>::type;

template<std::uint16_t BitsCount>
struct ValueType;

template<> struct ValueType<4>
{
    using type = std::uint8_t;
    static constexpr std::uint16_t mask = 0x000f;
};
template<> struct ValueType<8>
{
    using type = std::uint8_t;
    static constexpr std::uint16_t mask = 0x00ff;
};
template<> struct ValueType<12>
{
    using type = std::uint16_t;
    static constexpr std::uint16_t mask = 0x0fff;
};
template<> struct ValueType<16>
{
    using type = std::uint16_t;
    static constexpr std::uint16_t mask = 0xffff;
};

template<unsigned I_, std::uint16_t BitsCount_, typename Tag_>
struct Value
{
    static_assert(I_ < 4);
    static_assert(BitsCount_ > 0);
    static_assert((BitsCount_ % 4) == 0);
    static_assert((BitsCount_ / 8) <= 2);

    using Tag = Tag_;
    static constexpr unsigned I = I_;
    static constexpr unsigned BitsCount = BitsCount_;
    using type = typename ValueType<BitsCount>::type;

    constexpr type fetch(std::uint16_t v) const
    {
        constexpr std::uint16_t shift = ((4 - I) * 4 - BitsCount_);
        constexpr std::uint16_t mask = (ValueType<BitsCount>::mask << shift);
        return type((v & mask) >> shift);
    }
};

template<typename ValueTuple>
struct as_parameters_tuple;

template<typename... Vs>
struct as_parameters_tuple<std::tuple<Vs...>>
{
    using type = std::tuple<typename Vs::type...>;
};

template<typename ValueTuple>
using as_parameters_tuple_t = typename as_parameters_tuple<ValueTuple>::type;

template<typename ValueTuple>
constexpr auto as_arguments_tuple(std::uint16_t value)
{
    using Size = std::tuple_size<ValueTuple>;
    return [value]<std::size_t... Is>(std::index_sequence<Is...>)
    {
        using R = as_parameters_tuple_t<ValueTuple>;
        const ValueTuple vs;
        return R(std::get<Size::value - Is - 1>(vs).fetch(value)...);
    }(std::make_index_sequence<Size::value>());
}

template<unsigned I, typename Tag>
using default_tuple = std::tuple<Value<I, 4, Tag>>;

template<typename V1, typename V2>
constexpr auto add_same()
{
    static_assert(std::is_same_v<typename V1::Tag, typename V2::Tag>);
    return Value<V1::I, V1::BitsCount + V2::BitsCount, typename V1::Tag>();
}

template<typename V1, typename V2>
using add_same_t = decltype(add_same<V1, V2>());

template<typename T, typename V, typename... Ps>
struct replace_last_in_tuple;

template<typename T1, typename V, typename... Ps>
struct replace_last_in_tuple<std::tuple<T1>, V, Ps...>
{
    using type = std::tuple<Ps..., V>;
};

template<typename T0, typename... Ts, typename V, typename... Ps>
struct replace_last_in_tuple<std::tuple<T0, Ts...>, V, Ps...>
    : replace_last_in_tuple<std::tuple<Ts...>, V, Ps..., T0>
{
};

template<typename T, typename V>
using replace_last_in_tuple_t = typename replace_last_in_tuple<T, V>::type;

template<typename F, typename Tuple, typename... Ps>
struct filter;

template<typename F, typename... Ps>
struct filter<F, std::tuple<>, Ps...>
{
    using type = std::tuple<Ps...>;
};

template<typename F, typename A, typename... Ts, typename... Ps>
struct filter<F, std::tuple<A, Ts...>, Ps...>
{
    using Ok = decltype(std::declval<F>()(std::declval<A>()));
    using type = std::conditional_t<Ok::value
        , typename filter<F, std::tuple<Ts...>, A, Ps...>::type
        , typename filter<F, std::tuple<Ts...>, Ps...>::type>;
};

template<typename F, typename Tuple, typename... Ps>
using filter_t = typename filter<F, Tuple, Ps...>::type;

template<int>
struct tag_int {};

template<typename P>
struct is_tag_int : std::false_type {};

template<int I>
struct is_tag_int<tag_int<I>> : std::true_type {};

template<typename P>
constexpr bool is_tag_int_v = is_tag_int<P>::value;

template<unsigned Index, typename P>
using select_type_t = std::conditional_t<
      std::is_integral_v<P>
    , tag_int<Index>
    , P>;

template<typename P1, typename P2, typename P3, typename P4>
struct Match
{
    template<typename P>
    using Or_ = std::variant<P, std::uint8_t>;
    
    using Opcode_ = std::tuple<
        Or_<P1>, Or_<P2>, Or_<P3>, Or_<P4>>;
    
    Opcode_ opcode_;
    
    static constexpr auto do_()
    {
        auto reduce_op = []<typename ReduceTuple, typename Rhs>(ReduceTuple lhs, Rhs)
        {
            if constexpr (std::tuple_size_v<ReduceTuple> == 0)
            {
                return default_tuple<0, Rhs>();
            }
            else
            {
                using Last = std::tuple_element_t<std::tuple_size_v<ReduceTuple> -1, ReduceTuple>;
                using Prev = typename Last::Tag;
                if constexpr (std::is_same_v<Prev, Rhs>)
                {
                    return replace_last_in_tuple_t<ReduceTuple
                        , add_same_t<Last, Value<Last::I + 1, 4, Rhs>>>();
                }
                else
                {
                    return std::tuple_cat(lhs, default_tuple<
                        Last::I + Last::BitsCount / 4, Rhs>());
                }
            }
        };

        auto filter_op = []<typename T>(T)
        {
            using Tag = typename T::Tag;
            if constexpr (is_placeholder_v<Tag>)
            {
                if constexpr (Tag::value < 0) // _
                {
                    return std::false_type();
                }
                else
                {
                    return std::true_type();
                }
            }
            else if constexpr (is_tag_int_v<Tag>) // std::uint8_t
            {
                return std::false_type();
            }
            else
            {
                return std::true_type();
            }
        };

        using Parameters = std::tuple<
            select_type_t<0, P1>, select_type_t<1, P2>,
            select_type_t<2, P3>, select_type_t<3, P4>>;
        using Reduced = reduce_t<decltype(reduce_op), std::tuple<>, Parameters>;
        return filter_t<decltype(filter_op), Reduced>();
    }

    using UnpackTuple = decltype(do_());

    template<unsigned Index, int Tag>
    constexpr Match& set_byte(byte_placeholder<Tag> p)
    {
        std::get<Index>(opcode_) = {p};
        return *this;
    }

    template<unsigned Index>
#if (X_HAS_CONCEPTS())
    constexpr Match& set_byte(std::integral auto v)
#else
    constexpr Match& set_byte(int v)
#endif
    {
        std::get<Index>(opcode_) = {std::uint8_t(v)};
        return *this;
    }

    template<unsigned Index>
    constexpr bool check_impl(unsigned v) const
    {
        const auto& or_ = std::get<Index>(opcode_);
        assert(or_.index() != std::variant_npos);
        if (const std::uint8_t* to_match = std::get_if<std::uint8_t>(&or_))
        {
            return (*to_match == std::uint8_t(v));
        }
        // byte_placeholder<>.
        return true;
    }

    constexpr bool check(std::uint16_t opcode) const
    {
        const unsigned bytes[] =
        {
            (opcode & 0xF000u) >> 12u,
            (opcode & 0x0F00u) >>  8u,
            (opcode & 0x00F0u) >>  4u,
            (opcode & 0x000Fu) >>  0u,
        };
        return check_impl<0>(bytes[0])
            && check_impl<1>(bytes[1])
            && check_impl<2>(bytes[2])
            && check_impl<3>(bytes[3]);
    }


    template<typename F>
    constexpr void invoke(F&& f, std::uint16_t opcode) const
    {
        (void)std::apply(std::forward<F>(f)
            , as_arguments_tuple<UnpackTuple>(opcode));
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

template<typename F, typename M>
struct OpCodeOperation
{
    F op_;
    M match_;

    constexpr bool operator()(std::uint16_t opcode) const
    {
        if (match_.check(opcode))
        {
            match_.invoke(std::move(op_), opcode);
            return true;
        }
        return false;
    }
};

template<typename F, typename M>
OpCodeOperation(F, M) -> OpCodeOperation<F, M>;

#if (X_HAS_CONCEPTS())
template<typename T>
concept AnyByte = std::is_integral_v<T> || is_placeholder_v<T>;

template<typename F, typename R = void>
concept FunctionNoArg = std::invocable<F>
    && std::is_same_v<R, std::invoke_result_t<F>>;

template<typename F, typename P, typename R = void>
concept FunctionOneArg = std::invocable<F, P>
    && std::is_same_v<R, std::invoke_result_t<F, P>>;
#endif

#if (X_HAS_CONCEPTS())
template<typename B1, typename B2, typename B3, typename B4, typename F>
constexpr auto code(B1 b1, B2 b2, B3 b3, B4 b4, F f)
    requires AnyByte<B1>
          && AnyByte<B2>
          && AnyByte<B3>
          && AnyByte<B4>
    // #TODO: constrain F.
#else
template<typename B1, typename B2, typename B3, typename B4, typename F>
constexpr auto code(B1 b1, B2 b2, B3 b3, B4 b4, F f)
#endif
{
    using M = Match<B1, B2, B3, B4>;

    M match;
    match.template set_byte<0>(b1);
    match.template set_byte<1>(b2);
    match.template set_byte<2>(b3);
    match.template set_byte<3>(b4);
    return OpCodeOperation<F, M>{std::move(f), std::move(match)};
}

#if (X_HAS_CONCEPTS())
template<typename... Ops>
void match_opcode(std::uint16_t opcode, FunctionNoArg auto catch_all
    // error C3546: '...': there are no parameter packs available to expand
    //, FunctionOneArg<std::uint16_t/*parameter*/, bool/*return type*/> auto... ops)
    , Ops... ops)
#else
template<typename F, typename... Ops>
void match_opcode(std::uint16_t opcode, F catch_all, Ops... ops)
#endif
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
    
    pc_ += 2;
    execute_opcode(opcode);
}

void Chip8::execute_opcode(std::uint16_t opcode)
{
    match_opcode(opcode
        , []() { assert(false && "Unknown opcode."); }
        , code(0x0, 0x0, 0xE, 0x0, [&]()
        { /*CLS*/
            clear_display(); })
        , code(0x0, 0x0, 0xE, 0xE, [&]()
        { /*RET*/
            assert(sp_ > 0);
            --sp_;
            assert(sp_ < std::size(stack_));
            pc_ = stack_[sp_]; })
        , code(0x0, _n, _n, _n, [](std::uint16_t)
        { /* SYS addr. This instruction is only used on the old computers
            on which Chip-8 was originally implemented.
            It is ignored by modern interpreters. */ })
        , code(0x1, _n, _n, _n, [&](std::uint16_t nnn)
        { /* JP addr. */
            pc_ = nnn; })
        , code(0x2, _n, _n, _n, [&](std::uint16_t nnn)
        { /* CALL addr. */
            assert(sp_ < std::size(stack_));
            stack_[sp_] = pc_;
            ++sp_;
            pc_ = nnn; })
        , code(0x3, _x, _k, _k, [&](std::uint8_t x, std::uint8_t kk)
        { /* SE Vx, byte. */
            pc_ += ((V_[x] == kk) ? 2 : 0); })
        , code(0x4, _x, _k, _k, [&](std::uint8_t x, std::uint8_t kk)
        { /* SNE Vx, byte. */
            pc_ += ((V_[x] != kk) ? 2 : 0); })
        , code(0x5, _x, _y, 0x0, [&](std::uint8_t x, std::uint8_t y)
        { /* SE Vx, Vy. */
            pc_ += ((V_[x] == V_[y]) ? 2 : 0); })
        , code(0x6, _x, _k, _k, [&](std::uint8_t x, std::uint8_t kk)
        { /* LD Vx, byte. */
            V_[x] = kk; })
        , code(0x7, _x, _k, _k, [&](std::uint8_t x, std::uint8_t kk)
        { /* ADD Vx, byte. */
            V_[x] += kk; })
        , code(0x8, _x, _y, 0x0, [&](std::uint8_t x, std::uint8_t y)
        { /* LD Vx, Vy. */
            V_[x] = V_[y]; })
        , code(0x8, _x, _y, 0x1, [&](std::uint8_t x, std::uint8_t y)
        { /* OR Vx, Vy. */
            V_[x] = V_[x] | V_[y]; })
        , code(0x8, _x, _y, 0x2, [&](std::uint8_t x, std::uint8_t y)
        { /* AND Vx, Vy. */
            V_[x] = V_[x] & V_[y]; })
        , code(0x8, _x, _y, 0x3, [&](std::uint8_t x, std::uint8_t y)
        { /* XOR Vx, Vy. */
            V_[x] = V_[x] ^ V_[y]; })
        , code(0x8, _x, _y, 0x4, [&](std::uint8_t x, std::uint8_t y)
        { /* ADD Vx, Vy. */
            V_[x] = overflow_add(V_[x], V_[y], V_[0xF]); })
        , code(0x8, _x, _y, 0x5, [&](std::uint8_t x, std::uint8_t y)
        { /* SUB Vx, Vy. */
            V_[x] = overflow_sub(V_[x], V_[y], V_[0xF]);
            V_[0xF] = ((V_[0xF] == 0) ? 1 : 0); })
        , code(0x8, _x, _, 0x6, [&](std::uint8_t x)
        { /* SHR Vx {, Vy}. */
            V_[0xF] = V_[x] & 0x1;
            V_[x] >>= 1; })
        , code(0x8, _x, _y, 0x7, [&](std::uint8_t x, std::uint8_t y)
        { /* SUBN Vx, Vy. */
            V_[x] = overflow_sub(V_[y], V_[x], V_[0xF]);
            V_[0xF] = ((V_[0xF] == 0) ? 1 : 0); })
        , code(0x8, _x, _, 0xE, [&](std::uint8_t x)
        { /* SHL Vx {, Vy}. */
            V_[0xF] = V_[x] & 0x80;
            V_[x] <<= 1; })
        , code(0x9, _x, _y, 0x0, [&](std::uint8_t x, std::uint8_t y)
        { /* SNE Vx, Vy. */
            pc_ += ((V_[x] != V_[y]) ? 2 : 0); })
        , code(0xA, _n, _n, _n, [&](std::uint16_t nnn)
        { /* LD I, addr. */
            I_ = nnn; })
        , code(0xB, _n, _n, _n, [&](std::uint16_t nnn)
        { /* JP V0, addr. */
            pc_ = std::uint16_t(nnn + V_[0]); })
        , code(0xC, _x, _k, _k, [&](std::uint8_t x, std::uint8_t kk)
        { /* RND Vx, byte. */
            V_[x] = random_byte(*rd_) & kk; })
        , code(0xD, _x, _y, _n, [&](std::uint8_t x, std::uint8_t y, std::uint8_t n)
        { /* DRW Vx, Vy, nibble. */
            const bool collision = draw(V_[x], V_[y], {memory_ + I_, n});
            needs_redraw_ = true;
            V_[0xF] = (collision ? 1 : 0); })
        , code(0xE, _x, 0x9, 0xE, [&](std::uint8_t x)
        { /* SKP Vx. */
            pc_ += (keys_[V_[x]] ? 2 : 0); })
        , code(0xE, _x, 0xA, 0x1, [&](std::uint8_t x)
        { /* SKNP Vx. */
            pc_ += (!keys_[V_[x]] ? 2 : 0); })
        , code(0xF, _x, 0x0, 0x7, [&](std::uint8_t x)
        { /* LD Vx, DT. */
            V_[x] = delay_timer_; })
        , code(0xF, _x, 0x0, 0xA, [&](std::uint8_t x)
        { /* LD Vx, K. */
            wait_any_key(x); })
        , code(0xF, _x, 0x1, 0x5, [&](std::uint8_t x)
        { /* LD DT, Vx. */
            delay_timer_ = V_[x]; })
        , code(0xF, _x, 0x1, 0x8, [&](std::uint8_t x)
        { /* LD ST, Vx. */
            sound_timer_ = V_[x]; })
        , code(0xF, _x, 0x1, 0xE, [&](std::uint8_t x)
        { /* ADD I, Vx. */
            I_ += V_[x]; })
        , code(0xF, _x, 0x2, 0x9, [&](std::uint8_t x)
        { /* LD F, Vx. */
            const auto sprite_bytes = (std::size(kHexDigitsSprites) / 16);
            I_ = std::uint16_t(V_[x] * sprite_bytes); })
        , code(0xF, _x, 0x3, 0x3, [&](std::uint8_t x)
        { /* LD B, Vx. */
            std::uint8_t* ptr = (memory_ + I_);
            ptr[0] = (V_[x] / 100);
            ptr[1] = (V_[x] / 10) % 10;
            ptr[2] = (V_[x] % 100) % 10; })
        , code(0xF, _x, 0x5, 0x5, [&](std::uint8_t x)
        { /* LD [I], Vx. */
            std::copy(V_, V_ + x + 1
                , memory_ + I_); })
        , code(0xF, _x, 0x6, 0x5, [&](std::uint8_t x)
        { /* LD Vx, [I]. */
            std::copy(memory_ + I_, memory_ + I_ + x + 1
                , V_); })
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
    V_[vindex] = std::uint8_t(it - std::cbegin(keys_));
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

    chip8.pc_ = 0x200;
    chip8.needs_redraw_ = true;

    const bool read = ReadAllFileAsBinary(R"(roms/WIPEOFF)"
        , {chip8.memory_ + chip8.pc_, (std::size(chip8.memory_) - chip8.pc_)});
    assert(read);

    TickData data{chip8, renderer, picture, pixels};
    MainLoop(data);

    return 0;
}
