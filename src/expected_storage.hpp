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

#include <metal/expected_bits/unexpected.hpp>

namespace mtl {

    // �.�.8 unexpect tag [expected.unexpect]
    struct unexpect_t {};
    inline constexpr unexpect_t unexpect;

    template <typename T, typename E>
    requires(!std::is_same_v<T, unexpected<E>>) class expected;

    namespace detail {

        // expected_base
        template <typename T, typename E,
            bool value_is_trivially_destructible =
                std::is_trivially_destructible_v<T>,
            bool error_is_trivially_destructible =
                std::is_trivially_destructible_v<E>>
        struct expected_base {

            template <typename = void>
            requires(
                std::is_default_constructible_v<T>) constexpr expected_base()
                : _value(), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(in_place_t, Args&&... args)
                : _value(std::forward<Args>(args)...), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() = default;

            union {
                T _value;
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename T, typename E>
        struct expected_base<T, E, false, true> {

            template <typename = void>
            requires(
                std::is_default_constructible_v<T>) constexpr expected_base()
                : _value(), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(in_place_t, Args&&... args)
                : _value(std::forward<Args>(args)...), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() {
                if (_has_value) {
                    _value.~T();
                }
            }

            union {
                T _value;
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename T, typename E>
        struct expected_base<T, E, true, false> {

            template <typename = void>
            requires(
                std::is_default_constructible_v<T>) constexpr expected_base()
                : _value(), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(in_place_t, Args&&... args)
                : _value(std::forward<Args>(args)...), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() {
                if (!_has_value) {
                    _error.~unexpected<E>();
                }
            }

            union {
                T _value;
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename T, typename E>
        struct expected_base<T, E, false, false> {

            template <typename = void>
            requires(
                std::is_default_constructible_v<T>) constexpr expected_base()
                : _value(), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(in_place_t, Args&&... args)
                : _value(std::forward<Args>(args)...), _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() {
                if (_has_value) {
                    _value.~T();
                } else {
                    _error.~unexpected<E>();
                }
            }

            union {
                T _value;
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename E>
        struct expected_base<void, E, false, true> {

            constexpr expected_base() : _has_value(true) {}

            constexpr expected_base(in_place_t) : _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() = default;

            union {
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename E>
        struct expected_base<void, E, false, false> {

            constexpr expected_base() : _has_value(true) {}

            constexpr expected_base(in_place_t) : _has_value(true) {}

            template <typename... Args>
            constexpr expected_base(unexpect_t, Args&&... args)
                : _error(std::forward<Args>(args)...), _has_value(false) {}

            ~expected_base() {
                if (!_has_value) {
                    _error.~unexpected<E>();
                }
            }

            union {
                unexpected<E> _error;
            };
            bool _has_value;
        };

        // expected_storage
        template <typename T, typename E>
        struct expected_storage : expected_base<T, E> {

            using expected_base<T, E>::expected_base;

            constexpr expected_storage(const expected_storage& rhs) {
                if (rhs._has_value) {
                    construct_value(rhs._value);
                } else {
                    construct_error(rhs._error.value());
                }
            }

            // TODO: disable if not satisfied:
            // std::is_move_constructible_v<T>&&
            // std::is_move_constructible_v<E>)
            constexpr expected_storage(expected_storage&& rhs) noexcept(
                std::is_nothrow_move_constructible_v<T>&&
                    std::is_nothrow_move_constructible_v<E>) {
                if (rhs._has_value) {
                    construct_value(std::move(rhs._value));
                } else {
                    construct_error(std::move(rhs._error.value()));
                }
            }

            template <typename U, typename G>
            expected_storage(const expected<U, G>& rhs) {
                if (bool(rhs)) {
                    construct_value(*rhs);
                } else {
                    construct_error(rhs.error());
                }
            }

            template <typename U, typename G>
            expected_storage(expected<U, G>&& rhs) {
                if (bool(rhs)) {
                    construct_value(std::move(*rhs));
                } else {
                    construct_error(std::move(rhs.error()));
                }
            }

            template <typename... Args>
            constexpr void construct_value(Args&&... args) {
                new (std::addressof(_value)) T(std::forward<Args>(args)...);
                _has_value = true;
            }

            template <typename... Args>
            constexpr void construct_error(Args&&... args) {
                new (std::addressof(_error))
                    unexpected<E>(std::forward<Args>(args)...);
                _has_value = false;
            }
        };

        template <typename E>
        struct expected_storage<void, E> : expected_base<void, E> {

            using expected_base<void, E>::expected_base;

            constexpr expected_storage(const expected_storage& rhs) {
                if (rhs._has_value) {
                    construct_value();
                } else {
                    construct_error(rhs._error.value());
                }
            }

            // TODO: disable if not satisfied:
            // std::is_move_constructible_v<T>&&
            // std::is_move_constructible_v<E>)
            constexpr expected_storage(expected_storage&& rhs) noexcept(
                std::is_nothrow_move_constructible_v<E>) {
                if (rhs._has_value) {
                    construct_value());
                } else {
                    construct_error(std::move(rhs._error.value()));
                }
            }

            template <typename G>
            expected_storage(const expected<void, G>& rhs) {
                if (bool(rhs)) {
                    construct_value();
                } else {
                    construct_error(rhs.error());
                }
            }

            template <typename G>
            expected_storage(expected<void, G>&& rhs) {
                if (bool(rhs)) {
                    construct_value(std::move(*rhs));
                } else {
                    construct_error(std::move(rhs.error()));
                }
            }

            constexpr void construct_value() { _has_value = true; }

            template <typename... Args>
            constexpr void construct_error(Args&&... args) {
                new (std::addressof(_error))
                    unexpected<E>(std::forward<Args>(args)...);
                _has_value = false;
            }
        };

        // expected_default_constructors is used to delete the default copy
        // and move constructors when they are not available.
        template <typename T, typename E,
            bool enable_copy_constructor =
                (std::is_void_v<T> || std::is_copy_constructible_v<
                                          T>)&&std::is_copy_constructible_v<E>,
            bool enable_move_constructor =
                (std::is_void_v<T> || std::is_move_constructible_v<
                                          T>)&&std::is_move_constructible_v<E>>
        struct expected_default_constructors {
            expected_default_constructors() = default;
            expected_default_constructors(
                const expected_default_constructors&) = default;
            expected_default_constructors(
                expected_default_constructors&&) = default;
            // expected_default_constructors& operator=(
            //     const expected_default_constructors&) = default;
            // expected_default_constructors& operator=(
            //     expected_default_constructors&&) noexcept = default;
        };

        template <typename T, typename E>
        struct expected_default_constructors<T, E, false, true> {
            expected_default_constructors() = default;
            expected_default_constructors(
                const expected_default_constructors&) = delete;
            expected_default_constructors(
                expected_default_constructors&&) = default;
            // expected_default_constructors& operator=(
            //     const expected_default_constructors&) = default;
            // expected_default_constructors& operator=(
            //     expected_default_constructors&&) noexcept = default;
        };

        template <typename T, typename E>
        struct expected_default_constructors<T, E, true, false> {
            expected_default_constructors() = default;
            expected_default_constructors(
                const expected_default_constructors&) = default;
            expected_default_constructors(
                expected_default_constructors&&) = delete;
            // expected_default_constructors& operator=(
            //     const expected_default_constructors&) = default;
            // expected_default_constructors& operator=(
            //     expected_default_constructors&&) noexcept = default;
        };

        template <typename T, typename E>
        struct expected_default_constructors<T, E, false, false> {
            expected_default_constructors() = default;
            expected_default_constructors(
                const expected_default_constructors&) = delete;
            expected_default_constructors(
                expected_default_constructors&&) = delete;
            // expected_default_constructors& operator=(
            //     const expected_default_constructors&) = default;
            // expected_default_constructors& operator=(
            //     expected_default_constructors&&) noexcept = default;
        };

    } // namespace detail
} // namespace mtl
