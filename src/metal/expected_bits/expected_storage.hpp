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
    namespace detail {

        // expected_storage
        template <typename T, typename E>
        struct expected_storage {

            template <typename = void>
            requires(
                std::is_default_constructible_v<T>) constexpr expected_storage()
                : _value(), _has_value(true) {}

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

            template <typename U = T>
            constexpr expected_storage(U&& v) {
                construct_value(std::forward<U>(v));
            }

            ~expected_storage() {
                if (_has_value) {
                    _value.~T();
                } else {
                    _error.~unexpected<E>();
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

            union {
                T _value;
                unexpected<E> _error;
            };
            bool _has_value;
        };

        template <typename E>
        struct expected_storage<void, E> {

            constexpr expected_storage() : _has_value(true) {}

            constexpr expected_storage(const expected_storage& rhs) {
                if (rhs._has_value) {
                    construct_value();
                } else {
                    construct_error(rhs._error.value());
                }
            }

            constexpr expected_storage(expected_storage&& rhs) noexcept(
                std::is_nothrow_move_constructible_v<E>) {
                if (rhs._has_value) {
                    construct_value();
                } else {
                    construct_error(std::move(rhs._error.value()));
                }
            }
            ~expected_storage() {
                if (!_has_value) {
                    _error.~unexpected<E>();
                }
            }

            constexpr void construct_value() { _has_value = true; }

            template <typename... Args>
            constexpr void construct_error(Args&&... args) {
                new (std::addressof(_error))
                    unexpected<E>(std::forward<Args>(args)...);
                _has_value = false;
            }

            union {
                unexpected<E> _error;
            };
            bool _has_value;
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
