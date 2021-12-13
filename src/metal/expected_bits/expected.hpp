/*
    Copyright (c) 2021, Thierry Tremblay
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <metal/expected_bits/exception.hpp>
#include <metal/expected_bits/expected_storage.hpp>

namespace mtl {

    using std::in_place;

    // �.�.8 unexpect tag [expected.unexpect]
    struct unexpect_t {};
    inline constexpr unexpect_t unexpect;

    // �.�.4 Class template expected [expected.expected]
    template <typename T, typename E>
    requires(!std::is_same_v<T, unexpected<E>>) class expected
        : private detail::expected_storage<T, E>,
          private detail::expected_copy_constructor<T, E> {
    public:
        using value_type = T;
        using error_type = E;
        using unexpected_type = unexpected<E>;

        template <typename U>
        using rebind = expected<U, error_type>;

        // �.�.4.1, constructors
        constexpr expected() = default;
        constexpr expected(const expected& rhs) = default;

        // TODO: revise implementation, write unit tests. I needed this
        // constructor to write unit tests. Note: this constructor doesn't exist
        // for the void specialization. This is expected (lol).
        // TODO: test with non-moveable value type and test with moveable value
        // type.
        template <typename U = T>
        requires(std::is_constructible_v<T, U&&> &&
                 !std::is_same_v<std::remove_cvref_t<U>, in_place_t> &&
                 !std::is_same_v<std::remove_cvref_t<U>, expected<T, E>> &&
                 !std::is_same_v<std::remove_cvref_t<U>,
                     unexpected<E>>) explicit(!std::is_convertible_v<U&&,
                                              T>) constexpr expected(U&& v)
            : detail::expected_storage<T, E>(std::forward<U>(v)) {}

        //  �.�.4.2 Destructor [expected.object.dtor]
        ~expected() = default;

        // �.�.4.5 Observers [expected.object.observe]
        constexpr const T* operator->() const {
            return std::addressof(this->_value);
        }
        constexpr T* operator->() { return std::addressof(this->_value); }
        constexpr const auto& operator*() const& { return this->_value; }
        constexpr auto& operator*() & { return this->_value; }
        constexpr const auto&& operator*() const&& {
            return std::move(this->_value);
        }
        constexpr auto&& operator*() && { return std::move(this->_value); }
        constexpr explicit operator bool() const noexcept {
            return this->_has_value;
        }
        constexpr bool has_value() const noexcept { return this->_has_value; }

        // TODO: unit tests for value()
        template <typename U = T>
        requires(!std::is_void_v<T> &&
                 std::is_same_v<T, U>) constexpr const U& value() const& {
#if MTL_EXCEPTIONS
            if (!*this)
                throw bad_expected_access(error());
#endif
            return this->_value;
        }

        template <typename U = T>
        requires(
            !std::is_void_v<T> && std::is_same_v<T, U>) constexpr U& value() & {
#if MTL_EXCEPTIONS
            if (!*this)
                throw bad_expected_access(error());
#endif
            return this->_value;
        }

        template <typename U = T>
        requires(!std::is_void_v<T> &&
                 std::is_same_v<T, U>) constexpr const U&& value() const&& {
#if MTL_EXCEPTIONS
            if (!*this)
                throw bad_expected_access(error());
#endif
            return std::move(this->_value);
        }

        template <typename U = T>
        requires(!std::is_void_v<T> &&
                 std::is_same_v<T, U>) constexpr U&& value() && {
#if MTL_EXCEPTIONS
            if (!*this)
                throw bad_expected_access(error());
#endif
            return std::move(this->_value);
        }

        constexpr const E& error() const& { return this->_error.value(); }

        constexpr E& error() & { return this->_error.value(); }

        constexpr const E&& error() const&& {
            return std::move(this->_error.value());
        }

        constexpr E&& error() && { return std::move(this->_error.value()); }

        // TODO: unit tests for value_or()
        template <typename U>
        requires(std::is_copy_constructible_v<T>&&
                std::is_convertible_v<U&&, T>) constexpr T
            value_or(U&& value) const& {
            return bool(*this) ? **this
                               : static_cast<T>(std::forward<U>(value));
        }

        template <typename U>
        requires(std::is_move_constructible_v<T>&&
                std::is_convertible_v<U&&, T>) constexpr T
            value_or(U&& value) && {
            return bool(*this) ? std::move(**this)
                               : static_cast<T>(std::forward<U>(value));
        }

        //         template <typename = void>
        //         requires(std::is_move_constructible_v<T>&&
        //                 std::is_move_constructible_v<E>) constexpr
        //                 expected(expected&& rhs)
        //                 noexcept(std::is_nothrow_move_constructible_v<T>&&
        //                 std::is_nothrow_move_constructible_v<E>) {
        //             if (bool(rhs)) {
        //                 construct_value(std::move(*rhs));
        //             } else {
        //                 construct_error(std::move(rhs.error()));
        //             }
        //         }

        //         template <typename = void>
        //         requires(std::is_void_v<T>&&
        //                 std::is_move_constructible_v<E>) constexpr
        //                 expected(expected&& rhs)
        //                 noexcept(std::is_nothrow_move_constructible_v<E>) {
        //             if (bool(rhs)) {
        //                 construct_value();
        //             } else {
        //                 construct_error(std::move(rhs.error()));
        //             }
        //         }

        //         template <typename U, typename G>
        //         requires(
        //             std::is_constructible_v<T, const U&> &&
        //             !std::is_constructible_v<T, expected<U, G>&> &&
        //             !std::is_constructible_v<T, expected<U, G>&&> &&
        //             !std::is_constructible_v<T, const expected<U, G>&> &&
        //             !std::is_constructible_v<T, const expected<U, G>&&> &&
        //             !std::is_convertible_v<expected<U, G>&, T> &&
        //             !std::is_convertible_v<expected<U, G>&&, T> &&
        //             !std::is_convertible_v<const expected<U, G>&, T> &&
        //             !std::is_convertible_v<const expected<U, G>&&, T> &&
        //             std::is_constructible_v<E, const G&> &&
        //             !std::is_constructible_v<unexpected<E>, expected<U, G>&>
        //             && !std::is_constructible_v<unexpected<E>, expected<U,
        //             G>&&> && !std::is_constructible_v<unexpected<E>, const
        //             expected<U, G>&> &&
        //             !std::is_constructible_v<unexpected<E>, const expected<U,
        //             G>&&> && !std::is_convertible_v<expected<U, G>&,
        //             unexpected<E>> && !std::is_convertible_v<expected<U,
        //             G>&&, unexpected<E>> && !std::is_convertible_v<const
        //             expected<U, G>&, unexpected<E>> &&
        //             !std::is_convertible_v<const expected<U, G>&&,
        //                 unexpected<
        //                     E>>) explicit((!std::is_void_v<T> &&
        //                     !std::is_void_v<U> &&
        //                                       !std::is_convertible_v<const
        //                                       U&, T>) ||
        //                                   !std::is_convertible_v<const G&,
        //                                       E>) constexpr expected(const
        //                                       expected<U,
        //             G>& rhs) {
        //             if (bool(rhs)) {
        //                 construct_value(*rhs);
        //             } else {
        //                 construct_error(rhs.error());
        //             }
        //         }

        //         template <typename G>
        //         requires(std::is_void_v<T>) explicit(
        //             !std::is_convertible_v<const G&,
        //                 E>) constexpr expected(const expected<void, G>& rhs)
        //                 {
        //             if (bool(rhs)) {
        //                 construct_value();
        //             } else {
        //                 construct_error(rhs.error());
        //             }
        //         }

        //         template <typename U, typename G>
        //         requires(
        //             std::is_constructible_v<T, U&&> &&
        //             !std::is_constructible_v<T, expected<U, G>&> &&
        //             !std::is_constructible_v<T, expected<U, G>&&> &&
        //             !std::is_constructible_v<T, const expected<U, G>&> &&
        //             !std::is_constructible_v<T, const expected<U, G>&&> &&
        //             !std::is_convertible_v<expected<U, G>&, T> &&
        //             !std::is_convertible_v<expected<U, G>&&, T> &&
        //             !std::is_convertible_v<const expected<U, G>&, T> &&
        //             !std::is_convertible_v<const expected<U, G>&&, T> &&
        //             std::is_constructible_v<E, G&&> &&
        //             !std::is_constructible_v<unexpected<E>, expected<U, G>&>
        //             && !std::is_constructible_v<unexpected<E>, expected<U,
        //             G>&&> && !std::is_constructible_v<unexpected<E>, const
        //             expected<U, G>&> &&
        //             !std::is_constructible_v<unexpected<E>, const expected<U,
        //             G>&&> && !std::is_convertible_v<expected<U, G>&,
        //             unexpected<E>> && !std::is_convertible_v<expected<U,
        //             G>&&, unexpected<E>> && !std::is_convertible_v<const
        //             expected<U, G>&, unexpected<E>> &&
        //             !std::is_convertible_v<const expected<U, G>&&,
        //             unexpected<E>>)

        //             explicit((!std::is_void_v<T> && !std::is_void_v<U> &&
        //                          !std::is_convertible_v<U&&, T>) ||
        //                      !std::is_convertible_v<G&&,
        //                          E>) constexpr expected(expected<U, G>&& rhs)
        //                          {
        //             if (bool(rhs)) {
        //                 construct_value(std::move(*rhs));
        //             } else {
        //                 construct_error(std::move(rhs.error()));
        //             }
        //         }
        //         template <typename G>
        //         requires(std::is_void_v<T>) explicit(
        //             !std::is_convertible_v<G&&, E>) constexpr
        //             expected(expected<void, G>&& rhs) { if (bool(rhs)) {
        //                 construct_value();
        //             } else {
        //                 construct_error(std::move(rhs.error()));
        //             }
        //         }

        //         template <typename U = T>
        //         requires(!std::is_void_v<T> && std::is_constructible_v<T,
        //         U&&> &&
        //                  !std::is_same_v<std::remove_cvref_t<U>, in_place_t>
        //                  && !std::is_same_v<std::remove_cvref_t<U>,
        //                  expected<T, E>>
        //                  && !std::is_same_v<std::remove_cvref_t<U>,
        //                      unexpected<E>>)
        //                      explicit(!std::is_convertible_v<U&&,
        //                                               T>) constexpr
        //                                               expected(U&& v) {
        //             construct_value(std::forward<U>(v));
        //         }

        //         template <typename G = E>
        //         requires(std::is_constructible_v<E, const G&>) explicit(
        //             !std::is_convertible_v<const G&,
        //                 E>) constexpr expected(const unexpected<G>& e) {
        //             construct_error(e);
        //         }

        //         template <typename G = E>
        //         requires(std::is_constructible_v<E, G&&>) explicit(
        //             !std::is_convertible_v<G&&, E>) constexpr
        //             expected(unexpected<G>&&
        //                 e) noexcept(std::is_nothrow_constructible_v<E, G&&>)
        //                 {
        //             construct_error(std::move(e));
        //         }

        //         template <typename... Args>
        //         requires((std::is_void_v<T> && sizeof...(Args) == 0) ||
        //                  (!std::is_void_v<T> &&
        //                      std::is_constructible_v<T,
        //                          Args...>)) constexpr explicit
        //                          expected(in_place_t,
        //             Args&&... args) {
        //             construct_value(std::forward<Args>(args)...);
        //         }

        //         template <typename U, typename... Args>
        //         requires(!std::is_void_v<T> &&
        //                  std::is_constructible_v<T, initializer_list<U>&,
        //                      Args...>) constexpr explicit
        //                      expected(in_place_t,
        //             initializer_list<U> il, Args&&... args) {
        //             construct_value(il, std::forward<Args>(args)...);
        //         }

        //         template <typename... Args>
        //         requires(std::is_constructible_v<E,
        //             Args...>) constexpr explicit expected(unexpect_t,
        //             Args&&... args) { construct_error(in_place,
        //             std::forward<Args>(args)...);
        //         }

        //         template <typename U, typename... Args>
        //         requires(std::is_constructible_v<E, initializer_list<U>&,
        //             Args...>) constexpr explicit expected(unexpect_t,
        //             initializer_list<U> il, Args&&... args) {
        //             construct_error(in_place, il,
        //             std::forward<Args>(args)...);
        //         }

        //         ///@@@

        //         // �.�.4.3, assignment
        //         template <typename = void>
        //         requires((std::is_void_v<T> || (std::is_copy_assignable_v<T>
        //         &&
        //                                            std::is_copy_constructible_v<T>))
        //                                            &&
        //                  std::is_copy_assignable_v<E> &&
        //                  std::is_copy_constructible_v<E>

        //             ) expected&
        //         operator=(
        //             const expected& rhs) { // todo: noexcept() clause, not
        //             specified

        //             if (bool(*this)) {
        //                 if (bool(rhs)) {
        //                     _value = *rhs;
        //                 } else {
        //                     if (std::is_nothrow_copy_constructible_v<T>) {
        //                         _error.~unexpected<E>();
        //                         construct_value(*rhs);
        //                     } else if
        //                     (std::is_nothrow_move_constructible_v<T>) {
        //                         T temp(*rhs);
        //                         _error.~unexpected<E>();
        //                         construct_value(std::move(temp));
        //                     } else {
        // #if MTL_EXCEPTIONS
        //                         unexpected<E> temp(error());
        //                         _error.~unexpected<E>();
        //                         try {
        //                             construct_value(*rhs);
        //                         } catch (...) {
        //                             constuct_error(std::move(temp));
        //                             throw;
        //                         }
        // #else
        //                         construct_value(*rhs);
        // #endif
        //                     }
        //                 }
        //             } else {
        //                 if (bool(rhs)) {
        //                     if (std::is_nothrow_copy_constructible_v<E>) {
        //                         _value.~T();
        //                         construct_error(rhs.error());
        //                     } else if
        //                     (std::is_nothrow_move_constructible_v<E>) {
        //                         unexpected<E> temp(rhs.error());
        //                         _value.~T();
        //                         construct_error(std::move(temp));
        //                     } else {
        //                         T temp(*this);
        //                         _value.~T();
        // #if MTL_EXCEPTIONS
        //                         try {
        //                             construct_error(rhs.error());
        //                         } catch (...) {
        //                             construct_value(std::move(temp));
        //                             throw;
        //                         }
        // #else
        //                         construct_error(rhs.error());
        // #endif
        //                     }
        //                 } else {
        //                     _error = unexpected(rhs.error());
        //                 }
        //             }
        //             return *this;
        //         }

        //         //         expected& operator=(expected&&) = default;
        //         //         /*TODO noexcept(
        //         //             std::is_nothrow_move_assignable_v<T>&&
        //         // std::is_nothrow_move_constructible_v<T>);*/

        //         //         template <typename U = T,
        //         //                   std::enable_if_t<
        //         //                       !std::is_void_v<U> &&
        //         //                       !std::is_same_v<expected<T, E>,
        //         //                       std::remove_cvref_t<U>> &&
        //         // !std::conjunction_v<std::is_scalar<T>,
        //         //                                           std::is_same<T,
        //         // std::decay_t<U>>> &&
        //         //                       std::is_constructible_v<T, U>
        //         //                       /*TODO: && std::is_assignable_v<T&,
        //         U>*/
        //         //                       /*TODO: &&
        //         // std::is_nothrow_move_constructible_v<E>*/>*
        //         //                       = nullptr>
        //         //         expected& operator=(U&& other) {
        //         //             base::operator=(std::forward<U>(other));
        //         //             return *this;
        //         //         }

        //         //         template <typename G = E
        //         //                   // TODO:,
        //         //                   //
        //         // std::enable_if_t<std::is_nothrow_copy_constructible_v<E>
        //         //                   // && std::is_copy_assignable_v<E>>*
        //         =nullptr
        //         //                   >
        //         //         expected& operator=(const unexpected<G>& other) {
        //         //             base::operator=(other);
        //         //             return *this;
        //         //         }

        //         //         template <typename G = E
        //         //                   // TODO:,
        //         //                   //
        //         // std::enable_if_t<std::is_nothrow_move_constructible_v<E>
        //         //                   // && std::is_move_assignable_v<E>>*
        //         =nullptr
        //         //                   >
        //         //         expected& operator=(unexpected<G>&& other) {
        //         // base::operator=(std::forward<unexpected<G>>(other));
        //         //             return *this;
        //         //         }

        //         //         // �.�.4.4, modifiers

        //         //         template <typename U = T, typename... Args,
        //         //                   std::enable_if_t<!std::is_void_v<U>>*
        //         //                   =
        //         //                       nullptr>
        //         //         U& emplace(Args&&... args) {
        //         //             return
        //         base::emplace(std::forward<Args>(args)...);
        //         //         }

        //         //         template <typename U = T, typename V, typename...
        //         Args,
        //         //                   std::enable_if_t<!std::is_void_v<U>>*
        //         //                   =
        //         //                       nullptr>
        //         //         U& emplace(initializer_list<V> list, Args&&...
        //         args) {
        //         //             return base::emplace(list,
        //         //             std::forward<Args>(args)...);
        //         //         }

        //         //         // �.�.4.5, swap
        //         //         template <typename U = T
        //         //                   /* TODO
        //         //                       typename G = E,
        //         //                             std::enable_if_t<
        //         //                                 std::is_swappable_v<T> &&
        //         //
        //         //                                 TODO: Lvalues
        //         //                      of type T are
        //         // //
        //         // Swappable;
        //         // and
        //         //                                     std::is_swappable_v<E>
        //         &&
        //         //
        //         //                                     TODO:
        //         //                      Lvalues of type E
        //         // //
        //         // are
        //         //                      Swappable; and
        //         //                      std::is_void_v<U>
        //         //                      || (std::is_move_constructible_v<U>
        //         ||
        //         // std::is_move_constructible_v<G>)>*
        //         //                                  =
        //         //                      nullptr*/
        //         //                   >
        //         //         void swap(expected& other) noexcept(
        //         //             std::is_nothrow_move_constructible_v<T>&&
        //         //                 std::is_nothrow_swappable_v<T>&&
        //         // std::is_nothrow_move_constructible_v<E>&&
        //         //                         std::is_nothrow_swappable_v<E>) {
        //         //             base::swap(other);
        //         //         }

        //         // �.�.4.7, Expected equality operators
        //         template <typename T1, typename E1, typename T2, typename E2>
        //         friend constexpr bool operator==(
        //             const expected<T1, E1>& x, const expected<T2, E2>& y);
        //         template <typename T1, typename E1, typename T2, typename E2>
        //         friend constexpr bool operator!=(
        //             const expected<T1, E1>& x, const expected<T2, E2>& y);

        //         // �.�.4.8, Comparison with T
        //         template <typename T1, typename E1, typename T2>
        //         friend constexpr bool operator==(const expected<T1, E1>&,
        //         const T2&); template <typename T1, typename E1, typename T2>
        //         friend constexpr bool operator==(const T2&, const
        //         expected<T1, E1>&); template <typename T1, typename E1,
        //         typename T2> friend constexpr bool operator!=(const
        //         expected<T1, E1>&, const T2&); template <typename T1,
        //         typename E1, typename T2> friend constexpr bool
        //         operator!=(const T2&, const expected<T1, E1>&);

        //         // �.�.4.9, Comparison with unexpected<E>
        //         template <typename T1, typename E1, typename E2>
        //         friend constexpr bool operator==(
        //             const expected<T1, E1>&, const unexpected<E2>&);
        //         template <typename T1, typename E1, typename E2>
        //         friend constexpr bool operator==(
        //             const unexpected<E2>&, const expected<T1, E1>&);
        //         template <typename T1, typename E1, typename E2>
        //         friend constexpr bool operator!=(
        //             const expected<T1, E1>&, const unexpected<E2>&);
        //         template <typename T1, typename E1, typename E2>
        //         friend constexpr bool operator!=(
        //             const unexpected<E2>&, const expected<T1, E1>&);

        //         // �.�.4.10, Specialized algorithms
        //         template <typename T1, typename E1>
        //         friend void swap(expected<T1, E1>& x, expected<T1, E1>& y)
        //         noexcept(
        //             noexcept(x.swap(y)));
    };

    //     // template <typename T1, typename E1, typename T2, typename E2>
    //     // constexpr bool operator==(const expected<T1, E1>& x,
    //     //                           const expected<T2, E2>& y) {
    //     //     if (bool(x))
    //     //         return bool(y) && *x == *y;
    //     //     else
    //     //         return !bool(y) && x.error() == y.error();
    //     // }

    //     // template <typename E1, typename T2, typename E2>
    //     // constexpr bool operator==(const expected<void, E1>& x,
    //     //                           const expected<T2, E2>& y) {
    //     //     if (bool(x))
    //     //         return false;
    //     //     else
    //     //         return !bool(y) && x.error() == y.error();
    //     // }

    //     // template <typename T1, typename E1, typename E2>
    //     // constexpr bool operator==(const expected<T1, E1>& x,
    //     //                           const expected<void, E2>& y) {
    //     //     return y == x;
    //     // }

    //     // template <typename E1, typename E2>
    //     // constexpr bool operator==(const expected<void, E1>& x,
    //     //                           const expected<void, E2>& y) {
    //     //     if (bool(x))
    //     //         return bool(y);
    //     //     else

    //     //         return !bool(y) && x.error() == y.error();
    //     // }

    //     // template <typename T1, typename E1, typename T2, typename E2>
    //     // constexpr bool operator!=(const expected<T1, E1>& x,
    //     //                           const expected<T2, E2>& y) {
    //     //     return !(x == y);
    //     // }

    //     // template <typename T1, typename E1, typename T2>
    //     // constexpr bool operator==(const expected<T1, E1>& x, const T2& y)
    //     {
    //     //     return bool(x) ? *x == y : false;
    //     // }

    //     // template <typename T1, typename E1, typename T2>
    //     // constexpr bool operator==(const T2& x, const expected<T1, E1>& y)
    //     {
    //     //     return y == x;
    //     // }

    //     // template <typename T1, typename E1, typename T2>
    //     // constexpr bool operator!=(const expected<T1, E1>& x, const T2& y)
    //     {
    //     //     return !(x == y);
    //     // }

    //     // template <typename T1, typename E1, typename T2>
    //     // constexpr bool operator!=(const T2& x, const expected<T1, E1>& y)
    //     {
    //     //     return !(x == y);
    //     // }

    //     // template <typename T1, typename E1, typename E2>
    //     // constexpr bool operator==(const expected<T1, E1>& x,
    //     //                           const unexpected<E2>& y) {
    //     //     return bool(x) ? false : unexpected(x.error()) == y;
    //     // }

    //     // template <typename T1, typename E1, typename E2>
    //     // constexpr bool operator==(const unexpected<E2>& x,
    //     //                           const expected<T1, E1>& y) {
    //     //     return y == x;
    //     // }

    //     // template <typename T1, typename E1, typename E2>
    //     // constexpr bool operator!=(const expected<T1, E1>& x,
    //     //                           const unexpected<E2>& y) {
    //     //     return !(x == y);
    //     // }

    //     // template <typename T1, typename E1, typename E2>
    //     // constexpr bool operator!=(const unexpected<E2>& x,
    //     //                           const expected<T1, E1>& y) {
    //     //     return !(x == y);
    //     // }

    //     // template <typename T1, typename E1,
    //     //           std::enable_if_t<
    //     //               (std::is_void_v<std::remove_cvref_t<T1>> ||
    //     // std::is_move_constructible_v<T1>)&&std::is_swappable_v<T1>
    //     //                &&
    //     //               std::is_move_constructible_v<E1> &&
    //     //               std::is_swappable_v<E1>>* = nullptr>
    //     // void swap(expected<T1, E1>& x,
    //     //           expected<T1, E1>& y) noexcept(noexcept(x.swap(y))) {
    //     //     x.swap(y);
    //     // }

} // namespace mtl
