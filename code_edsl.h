#pragma once
#include <type_traits>
#include <utility>
#include <tuple>
#include <bit>

#include <cstdint>
#include <climits>
#include <cassert>

#if (__EMSCRIPTEN__)
#  define X_HAS_CONCEPTS() 0
#else
#  define X_HAS_CONCEPTS() 1
#endif

#if (X_HAS_CONCEPTS())
#  include <concepts>
#endif

#define X_SAMPLES

// General assumptions.
static_assert(CHAR_BIT == 8);
static_assert(std::endian::native == std::endian::little);

namespace edsl {
// To mark byte's "places" with unique identifier.
// 
// Example: code(0xf, _, _n, _n), [](std::uint8_t nn) {}.
// 
// This will match 2-bytes opcode that has form 0xfXXX,
// where last 2 XX(s) will be parsed to separate `nn` variable.
// `_` stands to "ignore everything" or "match anything".
template<int Tag_>
struct byte_placeholder : std::integral_constant<int, Tag_> {};

static constexpr byte_placeholder<-1> _;
static constexpr byte_placeholder<0> _n;
static constexpr byte_placeholder<1> _x;
static constexpr byte_placeholder<2> _y;
static constexpr byte_placeholder<3> _k;

// Given type P, check if it's any byte_placeholder<>.
template<typename P>
struct is_placeholder : std::false_type {};

template<int I>
struct is_placeholder<byte_placeholder<I>> : std::true_type {};

template<int I>
struct is_placeholder<const byte_placeholder<I>> : std::true_type {};

template<typename P>
constexpr bool is_placeholder_v = is_placeholder<P>::value;

#if defined(X_SAMPLES)
static_assert(is_placeholder_v<byte_placeholder<0>>);
static_assert(is_placeholder_v<decltype(_n)>);
static_assert(is_placeholder_v<decltype(_x)>);
static_assert(is_placeholder_v<decltype(_y)>);
static_assert(is_placeholder_v<decltype(_k)>);
static_assert(not is_placeholder_v<int>);
static_assert(not is_placeholder_v<void>);
#endif

// Yet another helper to create unique type from
// compile-time constant.
// Used to mark locations of int literals in
// `code(0xa, _n, 0xf, _x)`: for 0xa and 0xf
// tag_int<0> and tag_int<2> will be created.
template<int>
struct tag_int {};

// Given type P, check if it's any tag_int<>.
template<typename P>
struct is_tag_int : std::false_type {};

template<int I>
struct is_tag_int<tag_int<I>> : std::true_type {};

template<typename P>
constexpr bool is_tag_int_v = is_tag_int<P>::value;

#if defined(X_SAMPLES)
static_assert(is_tag_int_v<tag_int<0>>);
static_assert(is_tag_int_v<tag_int<1>>);
static_assert(not is_tag_int_v<int>);
#endif

// Given `code(0xa, _n, 0xf, _x)` with parameters of type
// (int, byte_placeholder<0>, int, byte_placeholder<1>),
// this helper will map raw int literals to unique tag_int<>.
// Needed to make sure that int literals will not be reduced to
// single match when doing reduce with placeholders. See below.
template<unsigned Index, typename P>
using select_tag_t = std::conditional_t<
      std::is_integral_v<P>
    , tag_int<Index>
    , P>;

#if defined(X_SAMPLES)
static_assert(std::is_same_v<
      select_tag_t<0, int>
    , tag_int<0>>);
static_assert(std::is_same_v<
      select_tag_t<1, decltype(_)>
    , decltype(_)>);
static_assert(std::is_same_v<
      select_tag_t<2, int>
    , tag_int<2>>);
#endif

// Same as std::accumulate()/fold, but for types/tuple.
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

#if defined(X_SAMPLES)
namespace example_ {
static constexpr auto reduce_op = [](auto t, auto V)
{ return std::tuple_cat(std::make_tuple(V), t); };

static_assert(std::is_same_v<
    reduce_t<decltype(reduce_op)
        , std::tuple<>                          // Initial state.
        , std::tuple<int, float, double, char>> // Input.
    , std::tuple<char, double, float, int>      // Result.
    >);
} // namespace example_
#endif

// Given tuple<A, ..., A1> replaces last A1 type with V.
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

#if defined(X_SAMPLES)
static_assert(std::is_same_v<
      replace_last_in_tuple_t<std::tuple<int>, char>
    , std::tuple<char>>);
static_assert(std::is_same_v<
      replace_last_in_tuple_t<std::tuple<int, bool>, char>
    , std::tuple<int, char>>);
static_assert(std::is_same_v<
      replace_last_in_tuple_t<std::tuple<int, bool, void>, char>
    , std::tuple<int, bool, char>>);
#endif

// Removes type T from tuple<T...> if predicate
// F for that type returns false.
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
        , typename filter<F, std::tuple<Ts...>, Ps..., A>::type
        , typename filter<F, std::tuple<Ts...>, Ps...>::type>;
};

template<typename F, typename Tuple, typename... Ps>
using filter_t = typename filter<F, Tuple, Ps...>::type;

#if defined(X_SAMPLES)
namespace example_ {
static constexpr auto filter_op = [](auto V)
{ return std::bool_constant<(sizeof(V) <= 2)>(); };

static_assert(std::is_same_v<
    filter_t<decltype(filter_op)
        , std::tuple<std::uint8_t, std::uint64_t, std::uint16_t, std::uint64_t>>
        , std::tuple<std::uint8_t, std::uint16_t>
    >);
} // namespace example_
#endif

// Given N bits count helps to identify bit mask to get first N bits
// from the value. Example: 0xfabc & 0x00ff -> 0x00bc.
// Bits are defined only for 2-bytes range and hex-quads.
// Also, gives minimum possible type to hold the value with N bits.
template<std::uint16_t BitsCount>
struct BitsTraits;

template<> struct BitsTraits<4>
{
    using type = std::uint8_t;
    static constexpr std::uint16_t mask = 0x000f;
};
template<> struct BitsTraits<8>
{
    using type = std::uint8_t;
    static constexpr std::uint16_t mask = 0x00ff;
};
template<> struct BitsTraits<12>
{
    using type = std::uint16_t;
    static constexpr std::uint16_t mask = 0x0fff;
};
template<> struct BitsTraits<16>
{
    using type = std::uint16_t;
    static constexpr std::uint16_t mask = 0xffff;
};

#if defined(X_SAMPLES)
static_assert((BitsTraits<4 >::mask & 0xabcd) == 0x000d);
static_assert((BitsTraits<8 >::mask & 0xabcd) == 0x00cd);
static_assert((BitsTraits<12>::mask & 0xabcd) == 0x0bcd);
static_assert((BitsTraits<16>::mask & 0xabcd) == 0xabcd);
static_assert(std::is_same_v<std::uint8_t,  BitsTraits<4 >::type>);
static_assert(std::is_same_v<std::uint8_t,  BitsTraits<8 >::type>);
static_assert(std::is_same_v<std::uint16_t, BitsTraits<12>::type>);
static_assert(std::is_same_v<std::uint16_t, BitsTraits<16>::type>);
#endif

// Given `code(0xa, _n, _x, _x)` we need a way to remember at
// compile time which placeholder maps to which part of the opcode.
// In the example above, 2-byte opcode is split into 4 parts
// and _n placeholder is binded to part number 1
// (starting from zero, left to right)
// and populated to 4 bits. _x placeholder starts from 2 and
// owns two 4-bits parts: 8 bits total.
// 
// For the example above Value<> holds all the context:
// `_n` is Value<1, 4, decltype(_n)> and
// `_x` is Value<2, 8, decltype(_x)>.
// 
// Once created, Value<> can answer to next questions:
// - what final type will be for given bits count/placeholder ?
// - given opcode v `fetch()` final value.
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
    using type = typename BitsTraits<BitsCount>::type;

    constexpr type fetch(std::uint16_t v) const
    {
        constexpr std::uint16_t shift = ((4 - I) * 4 - BitsCount_);
        constexpr std::uint16_t mask = (BitsTraits<BitsCount>::mask << shift);
        return type((v & mask) >> shift);
    }
};

#if defined(X_SAMPLES)
static_assert(Value<0, 4, decltype(_)>().fetch(0xabcd) == 0x000a);
static_assert(Value<1, 4, decltype(_)>().fetch(0xabcd) == 0x000b);
static_assert(Value<2, 4, decltype(_)>().fetch(0xabcd) == 0x000c);
static_assert(Value<3, 4, decltype(_)>().fetch(0xabcd) == 0x000d);

static_assert(Value<0, 8, decltype(_)>().fetch(0xabcd) == 0x00ab);
static_assert(Value<1, 8, decltype(_)>().fetch(0xabcd) == 0x00bc);
static_assert(Value<2, 8, decltype(_)>().fetch(0xabcd) == 0x00cd);

static_assert(Value<0, 12, decltype(_)>().fetch(0xabcd) == 0x0abc);
static_assert(Value<1, 12, decltype(_)>().fetch(0xabcd) == 0x0bcd);

static_assert(Value<0, 16, decltype(_)>().fetch(0xabcd) == 0xabcd);

static_assert(std::is_same_v<Value<0, 4,  decltype(_)>::type, std::uint8_t>);
static_assert(std::is_same_v<Value<1, 8,  decltype(_)>::type, std::uint8_t>);
static_assert(std::is_same_v<Value<1, 12, decltype(_)>::type, std::uint16_t>);
static_assert(std::is_same_v<Value<0, 16, decltype(_)>::type, std::uint16_t>);
#endif

// Given two Value<>(s) from same placeholder (Tags are equal),
// merge into single Value<> that has all bits.
// code(0xf, _n, _n, 0xa): two _n(s) need to be merged
// into single Value that starts from 1st part and is 8 bits long
// (2 x 4 bits from each _n).
template<typename V1, typename V2>
constexpr auto add_same_consecutive()
{
    static_assert(std::is_same_v<typename V1::Tag, typename V2::Tag>);
    static_assert(V2::I == (V1::I + 1));
    return Value<V1::I, V1::BitsCount + V2::BitsCount, typename V1::Tag>();
}

template<typename V1, typename V2>
using add_same_consecutive_t = decltype(add_same_consecutive<V1, V2>());

#if defined(X_SAMPLES)
static_assert(std::is_same_v<
      add_same_consecutive_t<Value<1, 4, decltype(_)>, Value<2, 4, decltype(_)>>
    , Value<1, 8, decltype(_)>
    >);
#endif

// Given tuple<Value<>, ...> returns tuple<T...>
// where T is Value<>::type, i.e, final type
// function needs to accept after `fetch()` call for give Value.
template<typename ValueTuple>
struct as_parameters_tuple;

template<typename... Vs>
struct as_parameters_tuple<std::tuple<Vs...>>
{
    using type = std::tuple<typename Vs::type...>;
};

template<typename ValueTuple>
using as_parameters_tuple_t = typename as_parameters_tuple<ValueTuple>::type;

#if defined(X_SAMPLES)
static_assert(std::is_same_v<
      as_parameters_tuple_t<std::tuple<
        Value<0, 4, decltype(_)>>>
    , std::tuple<std::uint8_t>>);
static_assert(std::is_same_v<
      as_parameters_tuple_t<std::tuple<
        Value<0, 4, decltype(_)>,
        Value<1, 4, decltype(_)>,
        Value<2, 4, decltype(_)>,
        Value<3, 4, decltype(_)>>>
    , std::tuple<std::uint8_t, std::uint8_t, std::uint8_t, std::uint8_t>>);
static_assert(std::is_same_v<
      as_parameters_tuple_t<std::tuple<
        Value<0, 8,  decltype(_)>,
        Value<2, 16, decltype(_)>>>
    , std::tuple<std::uint8_t, std::uint16_t>>);
#endif

// Given Values tuple<Value<>, ...> returns tuple<T...>
// where T is value after `fetch()` call for given `opcode`.
// Example: code(_n, _x, _y, _y) will create Value(s) tuple;
// as_arguments_tuple(0xabcd) will return std::tuple(0xa, 0xb, 0xcd).
template<typename ValueTuple>
constexpr auto as_arguments_tuple(std::uint16_t opcode)
{
    return [opcode]<std::size_t... Is>(std::index_sequence<Is...>)
    {
        using R = as_parameters_tuple_t<ValueTuple>;
        const ValueTuple vs;
        return R(std::get<Is>(vs).fetch(opcode)...);
    }(std::make_index_sequence<std::tuple_size_v<ValueTuple>>());
}

#if defined(X_SAMPLES)
static_assert(
    as_arguments_tuple<std::tuple<
        Value<0, 4, decltype(_)>,
        Value<1, 4, decltype(_)>,
        Value<2, 4, decltype(_)>,
        Value<3, 4, decltype(_)>>>
        (0xabcd)
    == std::make_tuple(
          std::uint8_t(0xa)
        , std::uint8_t(0xb)
        , std::uint8_t(0xc)
        , std::uint8_t(0xd)));

static_assert(
    as_arguments_tuple<std::tuple<
        Value<0, 4,  decltype(_)>,
        Value<1, 12, decltype(_)>>>
        (0xabcd)
    == std::make_tuple(
          std::uint8_t (0x000a)
        , std::uint16_t(0x0bcd)));
#endif

// The main part of this file.
// Helper that is parametrized by 4 types -
// all 4 parameters from code(...) function.
// In this example: code(0x1, _, _n, 0x2), types are:
// P1: int;
// P2: byte_placeholder<-1>; // decltype(_)
// P3: byte_placeholder< 0>;
// P4: int;
// 
// Given those type, helper can:
// (1) `check(opcode)` - returns true if `opcode` matches given rule.
// (2) `invoke(lambda, opcode)` - calls lambda with proper arguments
//     transformed following placeholders rules.
template<typename P1, typename P2, typename P3, typename P4>
struct Match
{
    using ActualParameters = std::tuple<P1, P2, P3, P4>;

    // Each of 4 parameters may be either any integral
    // literal or placeholder. In case it's int-like type,
    // it should be 4 bits long max. As result, 2 bytes
    // is enough to store any rule to match same 2 bytes opcode.
    // (placeholders do not need to be stored. Placeholder type
    // has all needed information to check & match).
    std::uint16_t rules_ = 0;
    
    static constexpr auto map_parameters_impl()
    {
        auto reduce_op = []<typename ReduceTuple, typename NextTag>(ReduceTuple lhs, NextTag)
        {
            if constexpr (std::tuple_size_v<ReduceTuple> == 0)
            {
                return std::tuple<Value<0, 4, NextTag>>();
            }
            else
            {
                using Last = std::tuple_element_t<std::tuple_size_v<ReduceTuple> -1, ReduceTuple>;
                using PrevTag = typename Last::Tag;
                if constexpr (std::is_same_v<PrevTag, NextTag>)
                {
                    return replace_last_in_tuple_t<ReduceTuple
                        , add_same_consecutive_t<Last, Value<Last::I + 1, 4, NextTag>>>();
                }
                else
                {
                    return std::tuple_cat(lhs
                        , std::tuple<Value<Last::I + Last::BitsCount / 4, 4, NextTag>>());
                }
            }
        };

        auto filter_op = []<typename T>(T)
        {
            using Tag = typename T::Tag;
            if constexpr (is_placeholder_v<Tag>)
            {
                // Ignore `_` placeholder that has negative tag.
                return std::bool_constant<(Tag::value >= 0)>();
            }
            else if constexpr (is_tag_int_v<Tag>) // std::uint8_t
            {
                // Ignore any raw int literals. We don't need to save them
                // in to variable.
                return std::false_type();
            }
            else
            {
                return std::true_type();
            }
        };

        using Tags = std::tuple<
              select_tag_t<0, P1> // Either unique tag_int<> or byte_placeholder<>.
            , select_tag_t<1, P2>
            , select_tag_t<2, P3>
            , select_tag_t<3, P4>>;

        // Group placeholders with same tags together.
        using Reduced = reduce_t<decltype(reduce_op), std::tuple<>, Tags>;
        // Remove not needed groups:
        // (1) group with "always ignore" _ tag.
        // (2) group/parameter with int literal that is know for the caller.
        return filter_t<decltype(filter_op), Reduced>();
    }

    using FinalArgsMetadata = decltype(map_parameters_impl());

    template<unsigned Index, int Tag>
    constexpr Match& set_byte(byte_placeholder<Tag>)
    {
        // Check that the actual type at Index position
        // is the same as specified in our template's parameters.
        static_assert(std::is_same_v<
              std::tuple_element_t<Index, ActualParameters>
            , byte_placeholder<Tag>>);

        // Nothing to store. Everything is in type.
        return *this;
    }

    template<unsigned Index>
#if (X_HAS_CONCEPTS())
    constexpr Match& set_byte(std::integral auto v)
#else
    constexpr Match& set_byte(int v)
#endif
    {
#if (X_HAS_CONCEPTS())
        // Check that the actual type at Index position
        // is the same as specified in our template's parameters.
        static_assert(std::is_same_v<
              std::tuple_element_t<Index, ActualParameters>
            , decltype(v)>);
#endif
        rules_ |= std::uint16_t(unsigned(v) << (16 - (Index + 1) * 4));
        return *this;
    }

    template<unsigned Index>
    constexpr bool check_impl(std::uint16_t opcode) const
    {
        if constexpr (is_placeholder_v<
            std::tuple_element_t<Index, ActualParameters>>)
        {
            // Named byte_placeholder<> matches anything.
            return true;
        }
        else
        {
            const std::uint16_t mask = std::uint16_t(0x000f << (16 - (Index + 1) * 4));
            return ((rules_ & mask) == (opcode & mask));
        }
    }

    constexpr bool check(std::uint16_t opcode) const
    {
        return check_impl<0>(opcode)
            && check_impl<1>(opcode)
            && check_impl<2>(opcode)
            && check_impl<3>(opcode);
    }

    template<typename F, typename User>
    constexpr void invoke(F&& f, User&& data, std::uint16_t opcode) const
    {
        auto call = [f_ = std::forward<F>(f)](auto&& data, auto... args)
        {
            if constexpr (std::is_invocable_v<decltype(f_)
                , decltype(data), decltype(args)...>)
            { // F(User&, ...) with user data.
                (void)f_(data, args...);
            }
            else if constexpr (std::is_invocable_v<decltype(f_)
                , decltype(args)...>)
            { // F(...) with NO user data.
                (void)f_(args...);
            }
            else
            { // Signature mismatch.
                []<bool false_always = false>() // Yet-another C++ hack.
                {
                    static_assert(false_always, "Callable does not match pattern.");
                }();
            }
        };

        std::apply(std::move(call)
            , std::tuple_cat(std::forward_as_tuple(std::forward<User>(data))
                , as_arguments_tuple<FinalArgsMetadata>(opcode)));
    }
};

#if defined(X_SAMPLES)
// All integer literals like in call `code(0xf, 0xa, 0xb, 0xc)`
// are collapsed to nothing since everything is constant/known.
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<int, int, int, int>::FinalArgsMetadata>
    , std::tuple<>>);
// `_` match any & ignore placeholder is collapsed to nothing too.
// code(_, _, _, _) -> []()
static_assert(std::is_same_v<as_parameters_tuple_t<
    Match<decltype(_), decltype(_), decltype(_), decltype(_)>::FinalArgsMetadata>
    , std::tuple<>>);
// All other (same) placeholders are combined together.
// code(_n, _n, _n, _n) -> [](std::uint16_t).
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<decltype(_n), decltype(_n), decltype(_n), decltype(_n)>::FinalArgsMetadata>
    , std::tuple<std::uint16_t>>);
// 12 bits (3 hex parts) are matched to minimum possible std::uint16_t.
// code(0x1, _n, _n, _n) -> [](std::uint16_t).
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<int, decltype(_n), decltype(_n), decltype(_n)>::FinalArgsMetadata>
    , std::tuple<std::uint16_t>>);
// code(0x1, 0x2, _n, _n) -> [](std::uint8_t).
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<int, int, decltype(_n), decltype(_n)>::FinalArgsMetadata>
    , std::tuple<std::uint8_t>>);
// code(0x1, _, _n, _) -> [](std::uint8_t).
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<int, decltype(_), decltype(_n), decltype(_)>::FinalArgsMetadata>
    , std::tuple<std::uint8_t>>);
// code(_x, _y, _n, _k) -> [](std::uint8_t, std::uint8_t, std::uint8_t, std::uint8_t).
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<decltype(_x), decltype(_y), decltype(_n), decltype(_k)>::FinalArgsMetadata>
    , std::tuple<std::uint8_t, std::uint8_t, std::uint8_t, std::uint8_t>>);
// code(0xa, _, 0xb, 0xc) -> []().
static_assert(std::is_same_v<as_parameters_tuple_t<
      Match<int, decltype(_), int, int>::FinalArgsMetadata>
    , std::tuple<>>);
#endif

#if defined(_MSC_VER)
#  define X_EBO() __declspec(empty_bases)
#else
#  define X_EBO()
#endif

// Invokable that holds match rule and handler to be called.
template<typename F, typename M>
struct X_EBO() OpCodeOperation : F
{
    M match_;

    template<typename User>
    constexpr bool try_invoke(User&& data, std::uint16_t opcode) &&
    {
        // static_assert(sizeof(*this) == 2);

        if (match_.check(opcode))
        {
            match_.invoke(std::move(static_cast<F&>(*this))
                , std::forward<User>(data)
                , opcode);
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

#if defined(X_SAMPLES)
static_assert(    code(0xa, 0xb, 0xc, 0xd, [] {}).match_.check(0xabcd));
static_assert(not code(0xa, 0xb, 0xc, 0xd, [] {}).match_.check(0x0000));
static_assert(code(_, 0xb, 0xc, 0xd, [] {}).match_.check(0xabcd));
static_assert(code(_, 0xb, 0xc, 0xd, [] {}).match_.check(0x0bcd));
static_assert(code(_, 0xb, 0xc, 0xd, [] {}).match_.check(0xbbcd));
static_assert(not code(_, 0xb, 0xc, 0xd, [] {}).match_.check(0xaacd));
static_assert(code(_, _, _, _, [] {}).match_.check(0xabcd));
static_assert(code(_, _, _, _, [] {}).match_.check(0xffff));
static_assert(code(_, _, _, 0xa, [] {}).match_.check(0xfffa));

// Empty callback -> size of single code(...) should be 2 bytes
// (same as std::uint16_t opcode).
namespace examples_ { struct empty {}; }

static_assert(sizeof(code(0xa, 0xb, 0xc, 0xd, examples_::empty())) == 2);
static_assert(sizeof(code(_  , 0xb, 0xc, 0xd, examples_::empty())) == 2);
static_assert(sizeof(code(_  , _n , 0xc, 0xd, examples_::empty())) == 2);
#endif

template<typename User, typename F, typename... Ops>
void match_opcode(User&& data, std::uint16_t opcode, F catch_all, Ops... ops)
{
    const bool handled = (std::move(ops).try_invoke(data, opcode) || ...);
    if (handled)
    {
        return;
    }
    if constexpr (std::is_invocable_v<F, decltype(data)>)
    {
        catch_all(data);
    }
    else
    {
        catch_all();
    }
}

#undef X_HAS_CONCEPTS
#undef X_EBO

#if defined(X_SAMPLES)
#  undef X_SAMPLES
#endif
} // namespace edsl
