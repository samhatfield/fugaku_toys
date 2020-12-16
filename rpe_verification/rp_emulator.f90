! Copyright 2015-2016 Andrew Dawson, Peter Dueben
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
! http:
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

MODULE rp_emulator
! A reduced-precision emulator.
!
! The `rpe_var` type is a simple container for a double precision
! floating point value.
!
    IMPLICIT NONE

    ! All definitions are private by default.
    PRIVATE

!-----------------------------------------------------------------------
! Module parameters and variables:
!-----------------------------------------------------------------------

    !: The Fortran kind of a single-precision and double-precision
    !: floating-point number.
    INTEGER, PARAMETER, PUBLIC :: RPE_SINGLE_KIND = kind(1.0), &
                                  RPE_DOUBLE_KIND = kind(1.0d0)

    !: The Fortran kind of the real data type used by the emulator
    !: (usually 64-bit double-precision).
    INTEGER, PARAMETER, PUBLIC :: RPE_REAL_KIND = RPE_DOUBLE_KIND
    !: The Fortran kind of an alternate real data type (usually 32-bit
    !: single-precision), should be single-precision if RPE_REAL_KIND
    !: is double-precision or double-precision if RPE_REAL_KIND is
    !: single-precision.
    INTEGER, PARAMETER, PUBLIC :: RPE_ALTERNATE_KIND = RPE_SINGLE_KIND

    !: Logical flag for turning the emulator on/off.
    LOGICAL, PUBLIC :: RPE_ACTIVE = .TRUE.

    !: The default number of bits to use in the reduced-precision significand.
    INTEGER, PUBLIC :: RPE_DEFAULT_SBITS = 52

    !: Logical flag for determining if IEEE half-precision rules should
    !: be used when operating on values with 10 bits in the significand.
    LOGICAL, PUBLIC :: RPE_IEEE_HALF = .FALSE.

    !: An internal value used to represent the case where a reduced-precision
    !: number has no specified precision yet.
    INTEGER, PARAMETER, PRIVATE :: RPE_SBITS_UNSPECIFIED = -1

!-----------------------------------------------------------------------
! Module derived-type definitions:
!-----------------------------------------------------------------------

    PUBLIC :: rpe_var
    TYPE :: rpe_var
    ! A reduced-precision floating-point number.
    !
    ! This type is a container for a floating-point number which is
    ! operated on in reduced precision.
    !
        INTEGER :: sbits = RPE_SBITS_UNSPECIFIED
        REAL(KIND=RPE_REAL_KIND) :: val
    END TYPE

    ! Create a public interface for constructing literal reduced
    ! precision values (rpe_var instances).
    PUBLIC rpe_literal
    INTERFACE rpe_literal
        MODULE PROCEDURE rpe_literal_real
        MODULE PROCEDURE rpe_literal_alternate
        MODULE PROCEDURE rpe_literal_integer
        MODULE PROCEDURE rpe_literal_long
    END INTERFACE

    ! Make the core emulator routines importable.
    PUBLIC :: apply_truncation, significand_bits

    PUBLIC ASSIGNMENT(=)
    INTERFACE ASSIGNMENT(=)
    ! Interfaces for the assignment operator with `rpe_type` instances.
    !
        MODULE PROCEDURE assign_rpe_rpe
        MODULE PROCEDURE assign_rpe_real
        MODULE PROCEDURE assign_rpe_alternate
        MODULE PROCEDURE assign_rpe_integer
        MODULE PROCEDURE assign_rpe_long
        MODULE PROCEDURE assign_real_rpe
        MODULE PROCEDURE assign_alternate_rpe
        MODULE PROCEDURE assign_integer_rpe
        MODULE PROCEDURE assign_long_rpe
    END INTERFACE

!-----------------------------------------------------------------------
! Interfaces for overloaded operators (external):
!-----------------------------------------------------------------------

    PUBLIC :: OPERATOR(+)
    INTERFACE OPERATOR(+)
        MODULE PROCEDURE add_rpe
        MODULE PROCEDURE add_rpe_rpe
        MODULE PROCEDURE add_rpe_integer
        MODULE PROCEDURE add_rpe_long
        MODULE PROCEDURE add_rpe_real
        MODULE PROCEDURE add_rpe_realalt
        MODULE PROCEDURE add_integer_rpe
        MODULE PROCEDURE add_long_rpe
        MODULE PROCEDURE add_real_rpe
        MODULE PROCEDURE add_realalt_rpe
    END INTERFACE OPERATOR(+)

    PUBLIC :: OPERATOR(-)
    INTERFACE OPERATOR(-)
        MODULE PROCEDURE sub_rpe
        MODULE PROCEDURE sub_rpe_rpe
        MODULE PROCEDURE sub_rpe_integer
        MODULE PROCEDURE sub_rpe_long
        MODULE PROCEDURE sub_rpe_real
        MODULE PROCEDURE sub_rpe_realalt
        MODULE PROCEDURE sub_integer_rpe
        MODULE PROCEDURE sub_long_rpe
        MODULE PROCEDURE sub_real_rpe
        MODULE PROCEDURE sub_realalt_rpe
    END INTERFACE OPERATOR(-)

    PUBLIC :: OPERATOR(*)
    INTERFACE OPERATOR(*)
        MODULE PROCEDURE mul_rpe_rpe
        MODULE PROCEDURE mul_rpe_integer
        MODULE PROCEDURE mul_rpe_long
        MODULE PROCEDURE mul_rpe_real
        MODULE PROCEDURE mul_rpe_realalt
        MODULE PROCEDURE mul_integer_rpe
        MODULE PROCEDURE mul_long_rpe
        MODULE PROCEDURE mul_real_rpe
        MODULE PROCEDURE mul_realalt_rpe
    END INTERFACE OPERATOR(*)

    PUBLIC :: OPERATOR(/)
    INTERFACE OPERATOR(/)
        MODULE PROCEDURE div_rpe_rpe
        MODULE PROCEDURE div_rpe_integer
        MODULE PROCEDURE div_rpe_long
        MODULE PROCEDURE div_rpe_real
        MODULE PROCEDURE div_rpe_realalt
        MODULE PROCEDURE div_integer_rpe
        MODULE PROCEDURE div_long_rpe
        MODULE PROCEDURE div_real_rpe
        MODULE PROCEDURE div_realalt_rpe
    END INTERFACE OPERATOR(/)

    PUBLIC :: OPERATOR(.GE.)
    INTERFACE OPERATOR(.GE.)
        MODULE PROCEDURE ge_rpe_rpe
        MODULE PROCEDURE ge_rpe_integer
        MODULE PROCEDURE ge_rpe_long
        MODULE PROCEDURE ge_rpe_real
        MODULE PROCEDURE ge_rpe_realalt
        MODULE PROCEDURE ge_integer_rpe
        MODULE PROCEDURE ge_long_rpe
        MODULE PROCEDURE ge_real_rpe
        MODULE PROCEDURE ge_realalt_rpe
    END INTERFACE OPERATOR(.GE.)

    PUBLIC :: OPERATOR(.LE.)
    INTERFACE OPERATOR(.LE.)
        MODULE PROCEDURE le_rpe_rpe
        MODULE PROCEDURE le_rpe_integer
        MODULE PROCEDURE le_rpe_long
        MODULE PROCEDURE le_rpe_real
        MODULE PROCEDURE le_rpe_realalt
        MODULE PROCEDURE le_integer_rpe
        MODULE PROCEDURE le_long_rpe
        MODULE PROCEDURE le_real_rpe
        MODULE PROCEDURE le_realalt_rpe
    END INTERFACE OPERATOR(.LE.)

    PUBLIC :: OPERATOR(.GT.)
    INTERFACE OPERATOR(.GT.)
        MODULE PROCEDURE gt_rpe_rpe
        MODULE PROCEDURE gt_rpe_integer
        MODULE PROCEDURE gt_rpe_long
        MODULE PROCEDURE gt_rpe_real
        MODULE PROCEDURE gt_rpe_realalt
        MODULE PROCEDURE gt_integer_rpe
        MODULE PROCEDURE gt_long_rpe
        MODULE PROCEDURE gt_real_rpe
        MODULE PROCEDURE gt_realalt_rpe
    END INTERFACE OPERATOR(.GT.)

    PUBLIC :: OPERATOR(.LT.)
    INTERFACE OPERATOR(.LT.)
        MODULE PROCEDURE lt_rpe_rpe
        MODULE PROCEDURE lt_rpe_integer
        MODULE PROCEDURE lt_rpe_long
        MODULE PROCEDURE lt_rpe_real
        MODULE PROCEDURE lt_rpe_realalt
        MODULE PROCEDURE lt_integer_rpe
        MODULE PROCEDURE lt_long_rpe
        MODULE PROCEDURE lt_real_rpe
        MODULE PROCEDURE lt_realalt_rpe
    END INTERFACE OPERATOR(.LT.)

    PUBLIC :: OPERATOR(==)
    INTERFACE OPERATOR(==)
        MODULE PROCEDURE eq_rpe_rpe
        MODULE PROCEDURE eq_rpe_integer
        MODULE PROCEDURE eq_rpe_long
        MODULE PROCEDURE eq_rpe_real
        MODULE PROCEDURE eq_rpe_realalt
        MODULE PROCEDURE eq_integer_rpe
        MODULE PROCEDURE eq_long_rpe
        MODULE PROCEDURE eq_real_rpe
        MODULE PROCEDURE eq_realalt_rpe
    END INTERFACE OPERATOR(==)

    PUBLIC :: OPERATOR(/=)
    INTERFACE OPERATOR(/=)
        MODULE PROCEDURE ne_rpe_rpe
        MODULE PROCEDURE ne_rpe_integer
        MODULE PROCEDURE ne_rpe_long
        MODULE PROCEDURE ne_rpe_real
        MODULE PROCEDURE ne_rpe_realalt
        MODULE PROCEDURE ne_integer_rpe
        MODULE PROCEDURE ne_long_rpe
        MODULE PROCEDURE ne_real_rpe
        MODULE PROCEDURE ne_realalt_rpe
    END INTERFACE OPERATOR(/=)

    PUBLIC :: OPERATOR(**)
    INTERFACE OPERATOR(**)
        MODULE PROCEDURE pow_rpe_rpe
        MODULE PROCEDURE pow_rpe_integer
        MODULE PROCEDURE pow_rpe_long
        MODULE PROCEDURE pow_rpe_real
        MODULE PROCEDURE pow_rpe_realalt
        MODULE PROCEDURE pow_integer_rpe
        MODULE PROCEDURE pow_long_rpe
        MODULE PROCEDURE pow_real_rpe
        MODULE PROCEDURE pow_realalt_rpe
    END INTERFACE OPERATOR(**)

!-----------------------------------------------------------------------
! Interfaces for overloaded intrinsic functions (external):
!-----------------------------------------------------------------------

    PUBLIC :: epsilon
    INTERFACE epsilon
        MODULE PROCEDURE epsilon_rpe
    END INTERFACE epsilon

    PUBLIC :: tiny
    INTERFACE tiny
        MODULE PROCEDURE tiny_rpe
    END INTERFACE tiny

    PUBLIC :: abs
    INTERFACE abs
        MODULE PROCEDURE abs_rpe
    END INTERFACE abs

    PUBLIC :: cos
    INTERFACE cos
        MODULE PROCEDURE cos_rpe
    END INTERFACE cos

    PUBLIC :: sin
    INTERFACE sin
        MODULE PROCEDURE sin_rpe
    END INTERFACE sin

    PUBLIC :: tan
    INTERFACE tan
        MODULE PROCEDURE tan_rpe
    END INTERFACE tan

    PUBLIC :: acos
    INTERFACE acos
        MODULE PROCEDURE acos_rpe
    END INTERFACE acos

    PUBLIC :: asin
    INTERFACE asin
        MODULE PROCEDURE asin_rpe
    END INTERFACE asin

    PUBLIC :: atan
    INTERFACE atan
        MODULE PROCEDURE atan_rpe
    END INTERFACE atan

    PUBLIC :: cosh
    INTERFACE cosh
        MODULE PROCEDURE cosh_rpe
    END INTERFACE cosh

    PUBLIC :: sinh
    INTERFACE sinh
        MODULE PROCEDURE sinh_rpe
    END INTERFACE sinh

    PUBLIC :: tanh
    INTERFACE tanh
        MODULE PROCEDURE tanh_rpe
    END INTERFACE tanh

    PUBLIC :: exp
    INTERFACE exp
        MODULE PROCEDURE exp_rpe
    END INTERFACE exp

    PUBLIC :: log
    INTERFACE log
        MODULE PROCEDURE log_rpe
    END INTERFACE log

    PUBLIC :: log10
    INTERFACE log10
        MODULE PROCEDURE log10_rpe
    END INTERFACE log10

    PUBLIC :: sqrt
    INTERFACE sqrt
        MODULE PROCEDURE sqrt_rpe
    END INTERFACE sqrt

    PUBLIC :: spacing
    INTERFACE spacing
        MODULE PROCEDURE spacing_rpe
    END INTERFACE spacing

    PUBLIC :: floor
    INTERFACE floor
        MODULE PROCEDURE floor_rpe
    END INTERFACE floor

    PUBLIC :: int
    INTERFACE int
        MODULE PROCEDURE int_rpe
    END INTERFACE int

    PUBLIC :: nint
    INTERFACE nint
        MODULE PROCEDURE nint_rpe
    END INTERFACE nint

    PUBLIC :: atan2
    INTERFACE atan2
        MODULE PROCEDURE atan2_rpe_rpe
        MODULE PROCEDURE atan2_rpe_real
        MODULE PROCEDURE atan2_real_rpe
    END INTERFACE atan2

    PUBLIC :: dim
    INTERFACE dim
        MODULE PROCEDURE dim_rpe_rpe
        MODULE PROCEDURE dim_rpe_real
        MODULE PROCEDURE dim_real_rpe
    END INTERFACE dim

    PUBLIC :: mod
    INTERFACE mod
        MODULE PROCEDURE mod_rpe_rpe
        MODULE PROCEDURE mod_rpe_real
        MODULE PROCEDURE mod_real_rpe
    END INTERFACE mod

    PUBLIC :: nearest
    INTERFACE nearest
        MODULE PROCEDURE nearest_rpe_rpe
        MODULE PROCEDURE nearest_rpe_real
        MODULE PROCEDURE nearest_real_rpe
    END INTERFACE nearest

    PUBLIC :: sign
    INTERFACE sign
        MODULE PROCEDURE sign_rpe_rpe
        MODULE PROCEDURE sign_rpe_real
        MODULE PROCEDURE sign_real_rpe
    END INTERFACE sign

    PUBLIC :: min
    INTERFACE min
        MODULE PROCEDURE min_rpe_rpe
        MODULE PROCEDURE min_rpe_real
        MODULE PROCEDURE min_real_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe
    END INTERFACE min

    PUBLIC :: max
    INTERFACE max
        MODULE PROCEDURE max_rpe_rpe
        MODULE PROCEDURE max_rpe_real
        MODULE PROCEDURE max_real_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe
        MODULE PROCEDURE max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe
    END INTERFACE max

    PUBLIC :: minval
    INTERFACE minval
        MODULE PROCEDURE minval_rpe_1d
        MODULE PROCEDURE minval_rpe_2d
        MODULE PROCEDURE minval_rpe_3d
        MODULE PROCEDURE minval_rpe_4d
        MODULE PROCEDURE minval_rpe_5d
    END INTERFACE minval

    PUBLIC :: maxval
    INTERFACE maxval
        MODULE PROCEDURE maxval_rpe_1d
        MODULE PROCEDURE maxval_rpe_2d
        MODULE PROCEDURE maxval_rpe_3d
        MODULE PROCEDURE maxval_rpe_4d
        MODULE PROCEDURE maxval_rpe_5d
    END INTERFACE maxval

    PUBLIC :: sum
    INTERFACE sum
        MODULE PROCEDURE sum_rpe_1d
        MODULE PROCEDURE sum_rpe_2d
        MODULE PROCEDURE sum_rpe_3d
        MODULE PROCEDURE sum_rpe_4d
        MODULE PROCEDURE sum_rpe_5d
    END INTERFACE sum

!-----------------------------------------------------------------------
! Interfaces for other extensions (external):
!-----------------------------------------------------------------------

    PUBLIC :: huge
    INTERFACE huge
        MODULE PROCEDURE huge_rpe
    END INTERFACE huge

CONTAINS

!-----------------------------------------------------------------------
! Core emulator procedures:
!-----------------------------------------------------------------------

    ELEMENTAL SUBROUTINE apply_truncation (x)
    ! Reduce the precision of a `rpe_type` instance.
    !
    ! Truncates the given floating-point number significand to the
    ! number of bits defined by the `sbits` member of the number. If the
    ! `sbits` attribute is not set it will truncate to the number of
    ! bits specified by the current value of `RPE_DEFAULT_SBITS`.
    !
    ! If the module variable RPE_ACTIVE is false this subroutine returns
    ! the unaltered input value, it only performs the bit truncation if
    ! RPE_ACTIVE is true.
    !
    ! Argument:
    !
    ! * x: type(rpe_type) [input/output]
    ! The `rpe_type` instance to truncate.
    !
        TYPE(rpe_var), INTENT(INOUT) :: x
        REAL(KIND=RPE_DOUBLE_KIND) :: y
        INTEGER :: n
        IF (RPE_ACTIVE) THEN
            ! Cast the input to a double-precision value.
            y = REAL(x%val, RPE_DOUBLE_KIND)
            IF (x%sbits == RPE_SBITS_UNSPECIFIED) THEN
                ! If the input does not have a specified precision then assume
                ! the default precision. This is does not fix the precision of
                ! the input variable, it will still use whatever is specified
                ! as the default, even if that changes later.
                n = RPE_DEFAULT_SBITS
            ELSE
                n = x%sbits
            END IF
            x%val = truncate_significand(y, n)
        END IF
    END SUBROUTINE apply_truncation

    ELEMENTAL FUNCTION truncate_significand (x, n) RESULT (t)
    ! Truncate the significand of a double precision floating point
    ! number to a specified number of bits.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_DOUBLE_KIND) [input]
    ! The double precision number to truncate.
    !
    ! * n: integer [input]
    ! The number of bits to truncate the significand to.
    !
    ! Returns:
    !
    ! * t: real(kind=RPE_DOUBLE_KIND)
    ! A double precision number representing `x` truncated to `n`
    ! bits in the significand.
    !
        REAL(KIND=RPE_DOUBLE_KIND), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: n
        REAL(KIND=RPE_DOUBLE_KIND) :: t
        INTEGER :: lmtb
        INTEGER(KIND=8), PARAMETER :: two = 2
        INTEGER(KIND=8), PARAMETER :: zero_bits = 0
        INTEGER(KIND=8) :: bits
        ! The left-most truncated bit is the last bit that will be truncated
        ! (counting from 0 at the right-most bit). Double precision values
        ! have 52 bits in their significand.
        lmtb = 52 - n - 1
        IF (lmtb >= 0) THEN
            ! Copy the double-precision bit representation of the input
            ! into an integer so it can be manipulated:
            bits = TRANSFER(x, bits)
            ! Round the number up first if required according to IEEE 754
            ! specifications.
            IF (BTEST(bits, lmtb)) THEN
                IF (IAND(bits, two ** (lmtb + 1) - 1) == two ** lmtb) THEN
                    ! We are truncating a number half-way between two
                    ! representations so we must round to the nearest even
                    ! representation.
                    IF (BTEST(bits, lmtb + 1)) THEN
                        bits = bits + two ** (lmtb + 1)
                    END IF
                ELSE
                    ! The left-most truncated bit is set and we are not
                    ! half-way between two representations so we need to
                    ! round to the nearest representation.
                    bits = bits + two ** (lmtb + 1)
                END IF
            END IF
            ! Move rounding_bit + 1 bits from the number zero (all bits
            ! set to zero) into the target to truncate at the given
            ! number of bits.
            CALL MVBITS (zero_bits, 0, lmtb + 1, bits, 0)
            t = TRANSFER(bits, t)
            ! Special case for IEEE half-precision representation.
            IF (n == 10 .AND. RPE_IEEE_HALF) THEN
                t = adjust_ieee_half(t)
            END IF
        ELSE
            t = x
        END IF
    END FUNCTION truncate_significand

    ELEMENTAL FUNCTION adjust_ieee_half (x) RESULT (y)
    ! Adjust a floating-point number according to the IEEE half-precision
    ! specification by ensuring that numbers outside the range of
    ! half-precision are either overflowed or denormalized.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_DOUBLE_KIND) [input]
    ! The floating-point number to adjust.
    !
    ! Returns:
    !
    ! * y: real(kind=RPE_DOUBLE_KIND) [output]
    ! The adjusted floating-point value.
    !
        REAL(KIND=RPE_DOUBLE_KIND), INTENT(IN) :: x
        REAL(KIND=RPE_DOUBLE_KIND) :: y
        REAL(KIND=RPE_DOUBLE_KIND), PARAMETER :: two = 2.0_RPE_DOUBLE_KIND
        REAL(KIND=RPE_DOUBLE_KIND) :: sx, d1, d2
        IF (ABS(x) > 65504.0_RPE_DOUBLE_KIND) THEN
        ! Handle half-precision overflows.
            sx = SIGN(1.0_RPE_DOUBLE_KIND, x)
            d1 = HUGE(d1) * sx
            d2 = HUGE(d2) * sx
            ! This will deliberately cause a floating-point overflow exception
            ! and yield an infinity value with the same sign as the input if
            ! the exception is not handled.
            y = d1 + d2
        ELSE IF (ABS(x) < two ** (-14)) THEN
        ! Handle half-precision subnormal values.
            d1 = two ** (-24) ! step size for subnormal values
            d2 = MOD(ABS(x), d1)
            ! The new value is the old value rounded to the nearest subnormal
            ! step interval in the direction of zero.
            y = (ABS(x) - d2) * SIGN(1.0_RPE_DOUBLE_KIND, x)
            ! If the rounding should have gone away from zero then correct for
            ! it afterwards.
            IF (ABS(d2) > two ** (-25)) THEN
                y = y + d1 * SIGN(1.0_RPE_DOUBLE_KIND, y)
            END IF
        ELSE
        ! If the value is in range then just return it.
            y = x
        END IF
    END FUNCTION adjust_ieee_half

    ELEMENTAL FUNCTION significand_bits (x) RESULT (z)
    ! Retrieve the number of bits in a floating point significand.
    !
    ! This returns actual values for inputs of type `rpe_type` or `real`
    ! and 0 for anything else. This function is usually used to find the
    ! highest precision level involved in a floating-point calculation.
    !
    ! Arguments:
    !
    ! * x: class(*) [input]
    ! A scalar input of any type.
    !
    ! Returns:
    !
    ! * z: integer [output]
    ! The number of bits in the significand of the input floating-point
    ! value, or 0 if the input was not a floating-point value.
    !
        CLASS(*), INTENT(IN) :: x
        INTEGER :: z
        SELECT TYPE (x)
        TYPE IS (REAL(KIND=RPE_DOUBLE_KIND))
            z = 52
        TYPE IS (rpe_var)
            IF (x%sbits == RPE_SBITS_UNSPECIFIED) THEN
                z = RPE_DEFAULT_SBITS
            ELSE
                z = x%sbits
            END IF
        TYPE IS (REAL(KIND=RPE_SINGLE_KIND))
            z = 23
        CLASS DEFAULT
            z = 0
        END SELECT
    END FUNCTION significand_bits

    FUNCTION rpe_literal_real (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a real literal.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_REAL_KIND) [input]
    ! The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    ! The number of bits in the significand of the resulting reduced
    ! precision number. If not specified then the result will have the
    ! default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    ! An `rpe_var` instance representing the input literal at the given
    ! precision.
    !
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        INTEGER, OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_real

    FUNCTION rpe_literal_alternate (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a real literal.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input]
    ! The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    ! The number of bits in the significand of the resulting reduced
    ! precision number. If not specified then the result will have the
    ! default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    ! An `rpe_var` instance representing the input literal at the given
    ! precision.
    !
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        INTEGER, OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_alternate

    FUNCTION rpe_literal_integer (x, n) RESULT (z)
    ! Create an `rpe_var` instance from an integer literal.
    !
    ! Arguments:
    !
    ! * x: integer [input]
    ! The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    ! The number of bits in the significand of the resulting reduced
    ! precision number. If not specified then the result will have the
    ! default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    ! An `rpe_var` instance representing the input literal at the given
    ! precision.
    !
        INTEGER, INTENT(IN) :: x
        INTEGER, OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_integer

    FUNCTION rpe_literal_long (x, n) RESULT (z)
    ! Create an `rpe_var` instance from a long integer literal.
    !
    ! Arguments:
    !
    ! * x: integer(KIND=8) [input]
    ! The literal to transform to a reduced precision `rpe_var` instance.
    !
    ! * n: integer [input, optional]
    ! The number of bits in the significand of the resulting reduced
    ! precision number. If not specified then the result will have the
    ! default precision.
    !
    ! Returns:
    !
    ! * z: rpe_var
    ! An `rpe_var` instance representing the input literal at the given
    ! precision.
    !
        INTEGER(KIND=8), INTENT(IN) :: x
        INTEGER, OPTIONAL, INTENT(IN) :: n
        TYPE(rpe_var) :: z
        IF (PRESENT(n)) THEN
            z%sbits = n
        END IF
        z = x
    END FUNCTION rpe_literal_long


!-----------------------------------------------------------------------
! Overloaded assignment definitions:
!-----------------------------------------------------------------------

    ELEMENTAL SUBROUTINE assign_rpe_rpe (r1, r2)
    ! Assign an `rpe_type` instance to another `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * r1: class(rpe_type) [input/output]
    ! An `rpe_type` instance to assign to.
    !
    ! * r2: class(rpe_type) [input]
    ! An `rpe_type` instance whose value will be assigned to `r1`.
    !
        TYPE(rpe_var), INTENT(INOUT) :: r1
        TYPE(rpe_var), INTENT(IN) :: r2
        r1%val = r2%val
        CALL apply_truncation (r1)
    END SUBROUTINE assign_rpe_rpe

    ELEMENTAL SUBROUTINE assign_rpe_real (rpe, x)
    ! Assign a real variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    ! An `rpe_type` instance to assign to.
    !
    ! * x: real(kind=RPE_REAL_KIND) [input]
    ! A real variable whose value will be assigned to `rpe`.
    !
        TYPE(rpe_var), INTENT(INOUT) :: rpe
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        rpe%val = x
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_real

    ELEMENTAL SUBROUTINE assign_rpe_alternate (rpe, x)
    ! Assign a real variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    ! An `rpe_type` instance to assign to.
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input]
    ! A real variable whose value will be assigned to `rpe`.
    !
        TYPE(rpe_var), INTENT(INOUT) :: rpe
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        rpe%val = x
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_alternate

    ELEMENTAL SUBROUTINE assign_rpe_integer (rpe, x)
    ! Assign an integer variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    ! An `rpe_type` instance to assign to.
    !
    ! * x: integer(kind=4) [input]
    ! An integer variable whose value will be assigned to `rpe`.
    !
        TYPE(rpe_var), INTENT(INOUT) :: rpe
        INTEGER(KIND=4), INTENT(IN) :: x
        rpe%val = x
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_integer

    ELEMENTAL SUBROUTINE assign_rpe_long (rpe, x)
    ! Assign a long integer variable to an `rpe_type` instance.
    !
    ! Arguments:
    !
    ! * rpe: class(rpe_type) [input/output]
    ! An `rpe_type` instance to assign to.
    !
    ! * x: integer(kind=8) [input]
    ! A long integer variable whose value will be assigned to `rpe`.
    !
        TYPE(rpe_var), INTENT(INOUT) :: rpe
        INTEGER(KIND=8), INTENT(IN) :: x
        rpe%val = x
        CALL apply_truncation (rpe)
    END SUBROUTINE assign_rpe_long

    ELEMENTAL SUBROUTINE assign_real_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a real variable.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_REAL_KIND) [input/output]
    ! A real variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    ! An `rpe_type` instance whose value will be assigned to `x`.
    !
        REAL(KIND=RPE_REAL_KIND), INTENT(INOUT) :: x
        TYPE(rpe_var), INTENT(IN) :: rpe
        x = rpe%val
    END SUBROUTINE assign_real_rpe

    ELEMENTAL SUBROUTINE assign_alternate_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a real variable.
    !
    ! Arguments:
    !
    ! * x: real(kind=RPE_ALTERNATE_KIND) [input/output]
    ! A real variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    ! An `rpe_type` instance whose value will be assigned to `x`.
    !
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(INOUT) :: x
        TYPE(rpe_var), INTENT(IN) :: rpe
        x = rpe%val
    END SUBROUTINE assign_alternate_rpe

    ELEMENTAL SUBROUTINE assign_integer_rpe (x, rpe)
    ! Assign an `rpe_type` instance to an integer variable.
    !
    ! Arguments:
    !
    ! * x: integer(kind=4) [input/output]
    ! An integer variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    ! An `rpe_type` instance whose value will be assigned to `x`.
    !
        INTEGER(KIND=4), INTENT(INOUT) :: x
        TYPE(rpe_var), INTENT(IN) :: rpe
        x = rpe%val
    END SUBROUTINE assign_integer_rpe

    ELEMENTAL SUBROUTINE assign_long_rpe (x, rpe)
    ! Assign an `rpe_type` instance to a long integer variable.
    !
    ! Arguments:
    !
    ! * x: integer(kind=8) [input/output]
    ! A long integer variable assign to.
    !
    ! * rpe: class(rpe_type) [input]
    ! An `rpe_type` instance whose value will be assigned to `x`.
    !
        INTEGER(KIND=8), INTENT(INOUT) :: x
        TYPE(rpe_var), INTENT(IN) :: rpe
        x = rpe%val
    END SUBROUTINE assign_long_rpe

!-----------------------------------------------------------------------
! Overloaded operator definitions (external):
!-----------------------------------------------------------------------

    !-------------------------------------------------------------------
    ! Overloaded definitions for (+):
    !

    ELEMENTAL FUNCTION add_rpe (x) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var) :: z
        z%sbits = significand_bits(x)
        z = +(x%val)
    END FUNCTION add_rpe

    ELEMENTAL FUNCTION add_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y%val
    END FUNCTION add_rpe_rpe

    ELEMENTAL FUNCTION add_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_integer

    ELEMENTAL FUNCTION add_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_long

    ELEMENTAL FUNCTION add_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_real

    ELEMENTAL FUNCTION add_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_realalt

    ELEMENTAL FUNCTION add_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_integer_rpe

    ELEMENTAL FUNCTION add_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_long_rpe

    ELEMENTAL FUNCTION add_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_real_rpe

    ELEMENTAL FUNCTION add_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (-):
    !

    ELEMENTAL FUNCTION sub_rpe (x) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var) :: z
        z%sbits = significand_bits(x)
        z = -(x%val)
    END FUNCTION sub_rpe

    ELEMENTAL FUNCTION sub_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y%val
    END FUNCTION sub_rpe_rpe

    ELEMENTAL FUNCTION sub_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_integer

    ELEMENTAL FUNCTION sub_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_long

    ELEMENTAL FUNCTION sub_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_real

    ELEMENTAL FUNCTION sub_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_realalt

    ELEMENTAL FUNCTION sub_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_integer_rpe

    ELEMENTAL FUNCTION sub_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_long_rpe

    ELEMENTAL FUNCTION sub_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_real_rpe

    ELEMENTAL FUNCTION sub_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (*):
    !

    ELEMENTAL FUNCTION mul_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y%val
    END FUNCTION mul_rpe_rpe

    ELEMENTAL FUNCTION mul_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_integer

    ELEMENTAL FUNCTION mul_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_long

    ELEMENTAL FUNCTION mul_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_real

    ELEMENTAL FUNCTION mul_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_realalt

    ELEMENTAL FUNCTION mul_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_integer_rpe

    ELEMENTAL FUNCTION mul_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_long_rpe

    ELEMENTAL FUNCTION mul_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_real_rpe

    ELEMENTAL FUNCTION mul_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (/):
    !

    ELEMENTAL FUNCTION div_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val / y%val
    END FUNCTION div_rpe_rpe

    ELEMENTAL FUNCTION div_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val / y
    END FUNCTION div_rpe_integer

    ELEMENTAL FUNCTION div_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val / y
    END FUNCTION div_rpe_long

    ELEMENTAL FUNCTION div_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val / y
    END FUNCTION div_rpe_real

    ELEMENTAL FUNCTION div_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val / y
    END FUNCTION div_rpe_realalt

    ELEMENTAL FUNCTION div_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x / y%val
    END FUNCTION div_integer_rpe

    ELEMENTAL FUNCTION div_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x / y%val
    END FUNCTION div_long_rpe

    ELEMENTAL FUNCTION div_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x / y%val
    END FUNCTION div_real_rpe

    ELEMENTAL FUNCTION div_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x / y%val
    END FUNCTION div_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (.GE.):
    !

    ELEMENTAL FUNCTION ge_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GE. y%val
    END FUNCTION ge_rpe_rpe

    ELEMENTAL FUNCTION ge_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GE. y
    END FUNCTION ge_rpe_integer

    ELEMENTAL FUNCTION ge_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GE. y
    END FUNCTION ge_rpe_long

    ELEMENTAL FUNCTION ge_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GE. y
    END FUNCTION ge_rpe_real

    ELEMENTAL FUNCTION ge_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GE. y
    END FUNCTION ge_rpe_realalt

    ELEMENTAL FUNCTION ge_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GE. y%val
    END FUNCTION ge_integer_rpe

    ELEMENTAL FUNCTION ge_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GE. y%val
    END FUNCTION ge_long_rpe

    ELEMENTAL FUNCTION ge_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GE. y%val
    END FUNCTION ge_real_rpe

    ELEMENTAL FUNCTION ge_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GE. y%val
    END FUNCTION ge_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (.LE.):
    !

    ELEMENTAL FUNCTION le_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LE. y%val
    END FUNCTION le_rpe_rpe

    ELEMENTAL FUNCTION le_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LE. y
    END FUNCTION le_rpe_integer

    ELEMENTAL FUNCTION le_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LE. y
    END FUNCTION le_rpe_long

    ELEMENTAL FUNCTION le_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LE. y
    END FUNCTION le_rpe_real

    ELEMENTAL FUNCTION le_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LE. y
    END FUNCTION le_rpe_realalt

    ELEMENTAL FUNCTION le_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LE. y%val
    END FUNCTION le_integer_rpe

    ELEMENTAL FUNCTION le_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LE. y%val
    END FUNCTION le_long_rpe

    ELEMENTAL FUNCTION le_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LE. y%val
    END FUNCTION le_real_rpe

    ELEMENTAL FUNCTION le_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LE. y%val
    END FUNCTION le_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (.GT.):
    !

    ELEMENTAL FUNCTION gt_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GT. y%val
    END FUNCTION gt_rpe_rpe

    ELEMENTAL FUNCTION gt_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GT. y
    END FUNCTION gt_rpe_integer

    ELEMENTAL FUNCTION gt_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GT. y
    END FUNCTION gt_rpe_long

    ELEMENTAL FUNCTION gt_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GT. y
    END FUNCTION gt_rpe_real

    ELEMENTAL FUNCTION gt_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .GT. y
    END FUNCTION gt_rpe_realalt

    ELEMENTAL FUNCTION gt_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GT. y%val
    END FUNCTION gt_integer_rpe

    ELEMENTAL FUNCTION gt_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GT. y%val
    END FUNCTION gt_long_rpe

    ELEMENTAL FUNCTION gt_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GT. y%val
    END FUNCTION gt_real_rpe

    ELEMENTAL FUNCTION gt_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .GT. y%val
    END FUNCTION gt_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (.LT.):
    !

    ELEMENTAL FUNCTION lt_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LT. y%val
    END FUNCTION lt_rpe_rpe

    ELEMENTAL FUNCTION lt_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LT. y
    END FUNCTION lt_rpe_integer

    ELEMENTAL FUNCTION lt_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LT. y
    END FUNCTION lt_rpe_long

    ELEMENTAL FUNCTION lt_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LT. y
    END FUNCTION lt_rpe_real

    ELEMENTAL FUNCTION lt_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val .LT. y
    END FUNCTION lt_rpe_realalt

    ELEMENTAL FUNCTION lt_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LT. y%val
    END FUNCTION lt_integer_rpe

    ELEMENTAL FUNCTION lt_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LT. y%val
    END FUNCTION lt_long_rpe

    ELEMENTAL FUNCTION lt_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LT. y%val
    END FUNCTION lt_real_rpe

    ELEMENTAL FUNCTION lt_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x .LT. y%val
    END FUNCTION lt_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (==):
    !

    ELEMENTAL FUNCTION eq_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val == y%val
    END FUNCTION eq_rpe_rpe

    ELEMENTAL FUNCTION eq_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val == y
    END FUNCTION eq_rpe_integer

    ELEMENTAL FUNCTION eq_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val == y
    END FUNCTION eq_rpe_long

    ELEMENTAL FUNCTION eq_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val == y
    END FUNCTION eq_rpe_real

    ELEMENTAL FUNCTION eq_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val == y
    END FUNCTION eq_rpe_realalt

    ELEMENTAL FUNCTION eq_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x == y%val
    END FUNCTION eq_integer_rpe

    ELEMENTAL FUNCTION eq_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x == y%val
    END FUNCTION eq_long_rpe

    ELEMENTAL FUNCTION eq_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x == y%val
    END FUNCTION eq_real_rpe

    ELEMENTAL FUNCTION eq_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x == y%val
    END FUNCTION eq_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (/=):
    !

    ELEMENTAL FUNCTION ne_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val /= y%val
    END FUNCTION ne_rpe_rpe

    ELEMENTAL FUNCTION ne_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        LOGICAL :: z
        z = x%val /= y
    END FUNCTION ne_rpe_integer

    ELEMENTAL FUNCTION ne_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val /= y
    END FUNCTION ne_rpe_long

    ELEMENTAL FUNCTION ne_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val /= y
    END FUNCTION ne_rpe_real

    ELEMENTAL FUNCTION ne_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        LOGICAL :: z
        z = x%val /= y
    END FUNCTION ne_rpe_realalt

    ELEMENTAL FUNCTION ne_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x /= y%val
    END FUNCTION ne_integer_rpe

    ELEMENTAL FUNCTION ne_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x /= y%val
    END FUNCTION ne_long_rpe

    ELEMENTAL FUNCTION ne_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x /= y%val
    END FUNCTION ne_real_rpe

    ELEMENTAL FUNCTION ne_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        LOGICAL :: z
        z = x /= y%val
    END FUNCTION ne_realalt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (**):
    !

    ELEMENTAL FUNCTION pow_rpe_rpe (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val ** y%val
    END FUNCTION pow_rpe_rpe

    ELEMENTAL FUNCTION pow_rpe_integer (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val ** y
    END FUNCTION pow_rpe_integer

    ELEMENTAL FUNCTION pow_rpe_long (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        INTEGER(KIND=8), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val ** y
    END FUNCTION pow_rpe_long

    ELEMENTAL FUNCTION pow_rpe_real (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val ** y
    END FUNCTION pow_rpe_real

    ELEMENTAL FUNCTION pow_rpe_realalt (x, y) RESULT (z)
        TYPE(rpe_var), INTENT(IN) :: x
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val ** y
    END FUNCTION pow_rpe_realalt

    ELEMENTAL FUNCTION pow_integer_rpe (x, y) RESULT (z)
        INTEGER, INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x ** y%val
    END FUNCTION pow_integer_rpe

    ELEMENTAL FUNCTION pow_long_rpe (x, y) RESULT (z)
        INTEGER(KIND=8), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x ** y%val
    END FUNCTION pow_long_rpe

    ELEMENTAL FUNCTION pow_real_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x ** y%val
    END FUNCTION pow_real_rpe

    ELEMENTAL FUNCTION pow_realalt_rpe (x, y) RESULT (z)
        REAL(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_var), INTENT(IN) :: y
        TYPE(rpe_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x ** y%val
    END FUNCTION pow_realalt_rpe

!-----------------------------------------------------------------------
! Overloaded intrinsic function definitions (external):
!-----------------------------------------------------------------------

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'epsilon':
    !

    FUNCTION epsilon_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = EPSILON(a%val)
    END FUNCTION epsilon_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tiny':
    !

    FUNCTION tiny_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = TINY(a%val)
    END FUNCTION tiny_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'abs':
    !

    ELEMENTAL FUNCTION abs_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = ABS(a%val)
    END FUNCTION abs_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'cos':
    !

    ELEMENTAL FUNCTION cos_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = COS(a%val)
    END FUNCTION cos_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sin':
    !

    ELEMENTAL FUNCTION sin_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = SIN(a%val)
    END FUNCTION sin_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tan':
    !

    ELEMENTAL FUNCTION tan_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = TAN(a%val)
    END FUNCTION tan_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'acos':
    !

    ELEMENTAL FUNCTION acos_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = ACOS(a%val)
    END FUNCTION acos_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'asin':
    !

    ELEMENTAL FUNCTION asin_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = ASIN(a%val)
    END FUNCTION asin_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'atan':
    !

    ELEMENTAL FUNCTION atan_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = ATAN(a%val)
    END FUNCTION atan_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'cosh':
    !

    ELEMENTAL FUNCTION cosh_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = COSH(a%val)
    END FUNCTION cosh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sinh':
    !

    ELEMENTAL FUNCTION sinh_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = SINH(a%val)
    END FUNCTION sinh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'tanh':
    !

    ELEMENTAL FUNCTION tanh_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = TANH(a%val)
    END FUNCTION tanh_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'exp':
    !

    ELEMENTAL FUNCTION exp_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = EXP(a%val)
    END FUNCTION exp_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'log':
    !

    ELEMENTAL FUNCTION log_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = LOG(a%val)
    END FUNCTION log_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'log10':
    !

    ELEMENTAL FUNCTION log10_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = LOG10(a%val)
    END FUNCTION log10_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sqrt':
    !

    ELEMENTAL FUNCTION sqrt_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = SQRT(a%val)
    END FUNCTION sqrt_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'spacing':
    !

    ELEMENTAL FUNCTION spacing_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        x%sbits = significand_bits(a)
        x = SPACING(a%val)
    END FUNCTION spacing_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'floor':
    !

    ELEMENTAL FUNCTION floor_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        INTEGER :: x
        x = FLOOR(a%val)
    END FUNCTION floor_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'int':
    !

    ELEMENTAL FUNCTION int_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        INTEGER :: x
        x = INT(a%val)
    END FUNCTION int_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'nint':
    !

    ELEMENTAL FUNCTION nint_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        INTEGER :: x
        x = NINT(a%val)
    END FUNCTION nint_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'atan2':
    !

    ELEMENTAL FUNCTION atan2_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = ATAN2(a%val, b%val)
    END FUNCTION atan2_rpe_rpe

    ELEMENTAL FUNCTION atan2_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = ATAN2(a%val, b)
    END FUNCTION atan2_rpe_real

    ELEMENTAL FUNCTION atan2_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = ATAN2(a, b%val)
    END FUNCTION atan2_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'dim':
    !

    ELEMENTAL FUNCTION dim_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = DIM(a%val, b%val)
    END FUNCTION dim_rpe_rpe

    ELEMENTAL FUNCTION dim_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = DIM(a%val, b)
    END FUNCTION dim_rpe_real

    ELEMENTAL FUNCTION dim_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = DIM(a, b%val)
    END FUNCTION dim_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'mod':
    !

    ELEMENTAL FUNCTION mod_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MOD(a%val, b%val)
    END FUNCTION mod_rpe_rpe

    ELEMENTAL FUNCTION mod_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MOD(a%val, b)
    END FUNCTION mod_rpe_real

    ELEMENTAL FUNCTION mod_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MOD(a, b%val)
    END FUNCTION mod_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'nearest':
    !

    ELEMENTAL FUNCTION nearest_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = NEAREST(a%val, b%val)
    END FUNCTION nearest_rpe_rpe

    ELEMENTAL FUNCTION nearest_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = NEAREST(a%val, b)
    END FUNCTION nearest_rpe_real

    ELEMENTAL FUNCTION nearest_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = NEAREST(a, b%val)
    END FUNCTION nearest_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sign':
    !

    ELEMENTAL FUNCTION sign_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = SIGN(a%val, b%val)
    END FUNCTION sign_rpe_rpe

    ELEMENTAL FUNCTION sign_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = SIGN(a%val, b)
    END FUNCTION sign_rpe_real

    ELEMENTAL FUNCTION sign_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = SIGN(a, b%val)
    END FUNCTION sign_real_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'min':
    !

    ELEMENTAL FUNCTION min_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MIN(a%val, b%val)
    END FUNCTION min_rpe_rpe

    ELEMENTAL FUNCTION min_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MIN(a%val, b)
    END FUNCTION min_rpe_real

    ELEMENTAL FUNCTION min_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MIN(a, b%val)
    END FUNCTION min_real_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe &
                       (a0, a1, a2) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val)
    END FUNCTION min_ma_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6, a7) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var), INTENT(IN) :: a7
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6), &
            significand_bits(a7))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val, &
            a7%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6, a7, a8) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var), INTENT(IN) :: a7
        TYPE(rpe_var), INTENT(IN) :: a8
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6), &
            significand_bits(a7), &
            significand_bits(a8))
        x = MIN(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val, &
            a7%val, &
            a8%val)
    END FUNCTION min_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'max':
    !

    ELEMENTAL FUNCTION max_rpe_rpe (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MAX(a%val, b%val)
    END FUNCTION max_rpe_rpe

    ELEMENTAL FUNCTION max_rpe_real (a, b) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MAX(a%val, b)
    END FUNCTION max_rpe_real

    ELEMENTAL FUNCTION max_real_rpe (a, b) RESULT (x)
        REAL(KIND=RPE_REAL_KIND), INTENT(IN) :: a
        TYPE(rpe_var), INTENT(IN) :: b
        TYPE(rpe_var) :: x
        x%sbits = MAX(significand_bits(a), significand_bits(b))
        x = MAX(a, b%val)
    END FUNCTION max_real_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe &
                       (a0, a1, a2) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val)
    END FUNCTION max_ma_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6, a7) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var), INTENT(IN) :: a7
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6), &
            significand_bits(a7))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val, &
            a7%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    ELEMENTAL FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe &
                       (a0, a1, a2, a3, a4, a5, a6, a7, a8) &
                       RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a0
        TYPE(rpe_var), INTENT(IN) :: a1
        TYPE(rpe_var), INTENT(IN) :: a2
        TYPE(rpe_var), INTENT(IN) :: a3
        TYPE(rpe_var), INTENT(IN) :: a4
        TYPE(rpe_var), INTENT(IN) :: a5
        TYPE(rpe_var), INTENT(IN) :: a6
        TYPE(rpe_var), INTENT(IN) :: a7
        TYPE(rpe_var), INTENT(IN) :: a8
        TYPE(rpe_var) :: x
        x%sbits = MAX(&
            significand_bits(a0), &
            significand_bits(a1), &
            significand_bits(a2), &
            significand_bits(a3), &
            significand_bits(a4), &
            significand_bits(a5), &
            significand_bits(a6), &
            significand_bits(a7), &
            significand_bits(a8))
        x = MAX(&
            a0%val, &
            a1%val, &
            a2%val, &
            a3%val, &
            a4%val, &
            a5%val, &
            a6%val, &
            a7%val, &
            a8%val)
    END FUNCTION max_ma_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'minval':
    !

    FUNCTION minval_rpe_1d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_1d

    FUNCTION minval_rpe_2d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_2d

    FUNCTION minval_rpe_3d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_3d

    FUNCTION minval_rpe_4d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_4d

    FUNCTION minval_rpe_5d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4), &
                                            SIZE(a, 5)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MINVAL(t)
    END FUNCTION minval_rpe_5d

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'maxval':
    !

    FUNCTION maxval_rpe_1d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_1d

    FUNCTION maxval_rpe_2d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_2d

    FUNCTION maxval_rpe_3d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_3d

    FUNCTION maxval_rpe_4d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_4d

    FUNCTION maxval_rpe_5d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4), &
                                            SIZE(a, 5)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = MAXVAL(t)
    END FUNCTION maxval_rpe_5d

    !-------------------------------------------------------------------
    ! Overloaded definitions for 'sum':
    !

    FUNCTION sum_rpe_1d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = SUM(t)
    END FUNCTION sum_rpe_1d

    FUNCTION sum_rpe_2d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = SUM(t)
    END FUNCTION sum_rpe_2d

    FUNCTION sum_rpe_3d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = SUM(t)
    END FUNCTION sum_rpe_3d

    FUNCTION sum_rpe_4d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = SUM(t)
    END FUNCTION sum_rpe_4d

    FUNCTION sum_rpe_5d (a) RESULT (x)
        TYPE(rpe_var), DIMENSION(:, :, :, :, :), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        REAL(KIND=RPE_REAL_KIND), DIMENSION(&
                                            SIZE(a, 1), &
                                            SIZE(a, 2), &
                                            SIZE(a, 3), &
                                            SIZE(a, 4), &
                                            SIZE(a, 5)) :: t
        x%sbits = MAXVAL(significand_bits(a))
        t = a
        x = SUM(t)
    END FUNCTION sum_rpe_5d

!-----------------------------------------------------------------------
! Other extensions (external):
!-----------------------------------------------------------------------

    !-------------------------------------------------------------------
    ! Hand-written overloaded definition for 'huge'
    !
    ! This function behaves differently than others for reduced
    ! precision. If we simply round the result of the HUGE intrinsic we
    ! will always yield infinity, since HUGE will return a value with
    ! a full significand.
    !
    ! This implementation performs truncation of the value returned by
    ! the HUGE intrinsic *without* doing rounding, which produces the
    ! correct result.
    !
    ! Note that we must also manually check if the emulator is turend on
    ! before performing the truncation (this is normally done by the
    ! overloaded assignment operator, but we are not using it here).
    !

    FUNCTION huge_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        INTEGER :: lmtb
        INTEGER(KIND=8), PARAMETER :: zero_bits = 0
        INTEGER(KIND=8) :: bits
        x%sbits = significand_bits(a)
        x%val = HUGE(a%val)
        IF (RPE_ACTIVE) THEN
            IF ((x%sbits == 10) .AND. (RPE_IEEE_HALF)) THEN
                ! For half precision emulation we need to specify the value
                ! explicitly, HUGE cannot do this in the absence of a native
                ! 16-bit real type:
                x%val = 65504
            ELSE
                ! Truncate to the required size without rounding, applying
                ! rounding will always round to infinity and is therefore no
                ! good for this purpose:
                lmtb = 52 - x%sbits - 1
                bits = TRANSFER(x%val, bits)
                CALL MVBITS (zero_bits, 0, lmtb + 1, bits, 0)
                x%val = TRANSFER(bits, x%val)
            END IF
        END IF
    END FUNCTION huge_rpe

END MODULE rp_emulator
