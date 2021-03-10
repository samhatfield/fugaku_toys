!===============================================================================
! A compilation test containing a number of common
!
! ECMWF/RIKEN-CCS
!
! Contributors:
! Sam Hatfield, ECMWF
! samuel.hatfield@ecmwf.int
!===============================================================================

program main
    implicit none

    ! Define precision for all real variables (defined by CPP macro)
    integer, parameter :: hp = 2
    
    real(hp) :: x = 1.0
    real(hp) :: v1(2) = (/ 1.0, 1.0 /)
    real(hp) :: m1(2,2)
    complex(hp) :: z = (1.0, 1.0)
    integer :: i

    m1(:,1) = (/ 1.0, 1.0 /)
    m1(:,2) = (/ 1.0, 1.0 /)

    ! Absolute value
    x = abs(x)

    ! Arc cosine
    x = acos(x)

    ! Convert/extract imaginary part of complex
    x = aimag(z)

    ! Truncate to whole number
    i = aint(x)

    ! Round to nearest whole number
    i = anint(x)

    ! Arc sine
    x = asin(x)

    ! Arc tangent
    x = atan(x)

    ! Arc tangent
    x = atan2(x, x)

    ! Number of bits in argument's type
    i = bit_size(x)

    ! Construct COMPLEX(KIND=1) value
    z = cmplx(x, x)

    ! Complex conjugate
    z = conjg(z)

    ! Cosine
    x = cos(x)

    ! Hyperbolic cosine
    x = cosh(x)

    ! Dot product
    x = dot_product(v1, v1)

    ! Error function
    x = erf(x)

    ! Complementary error function
    x = erfc(x)

    ! Exponential
    x = exp(x)

    ! Floor
    i = floor(x)

    ! Return fractional part
    x = fraction(x)

    ! Get largest representable value
    x = huge(x)

    ! Extract imaginary part of complex
    x = imag(z)

    ! Convert to INTEGER value truncated to whole number
    i = int(x)

    ! Natural logarithm
    x = log(x)

    ! Common logarithm
    x = log10(x)

    ! Matrix multiplication
    m1 = matmul(m1, m1)

    ! Maximum value
    x = max(x, x)

    ! Minimum value
    x = min(x, x)

    ! Convert to INTEGER value rounded to nearest whole number
    i = nint(x)

    ! Apply sign to magnitude
    x = sign(x, x)

    ! Sine
    x = sin(x)

    ! Hyperbolic sine
    x = sinh(x)

    ! Square root
    x = sqrt(x)

    ! Sum
    x = sum(v1)

    ! Tangent
    x = tan(x)

    ! Hyperbolic tangent
    x = tanh(x)

    ! Get smallest representable number
    x = tiny(x)

    ! Transpose
    m1 = transpose(m1)

end program main

