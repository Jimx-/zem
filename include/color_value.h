#ifndef _ZEM_COLOR_VALUE_H
#define _ZEM_COLOR_VALUE_H

#include <cassert>
#include <cstddef>

namespace zem {

struct ColorValue {
    unsigned char r, g, b, a;

    explicit ColorValue(unsigned char red, unsigned char green,
                        unsigned char blue, unsigned char alpha = 255)
        : r(red), g(green), b(blue), a(alpha)
    {}

    static inline ColorValue from_u32(unsigned int val)
    {
        int base_shift = 0;
        unsigned char alpha = 255;

        if (val >= 0x1000000) {
            base_shift = 8;
            alpha = val & 0xff;
        }

        return ColorValue{(unsigned char)((val >> (base_shift + 16)) & 0xff),
                          (unsigned char)((val >> (base_shift + 8)) & 0xff),
                          (unsigned char)((val >> base_shift) & 0xff), alpha};
    }

    inline unsigned int as_u32() const
    {
        return (a << 24) | (b << 16) | (g << 8) | r;
    }

    inline unsigned char operator[](size_t i) const
    {
        assert(i < 4);
        return *(&r + i);
    }

    inline unsigned char& operator[](size_t i)
    {
        assert(i < 4);
        return *(&r + i);
    }

    inline unsigned char* ptr() { return &r; }

    inline const unsigned char* ptr() const { return &r; }

    ColorValue operator+(const ColorValue& v) const
    {
        return ColorValue(r + v.r, g + v.g, b + v.b, a + v.a);
    }

    ColorValue& operator+=(const ColorValue& v)
    {
        r += v.r;
        g += v.g;
        b += v.b;
        a += v.a;

        return *this;
    }

    ColorValue operator-(const ColorValue& v) const
    {
        return ColorValue(r - v.r, g - v.g, b - v.b, a - v.a);
    }

    ColorValue operator*(const ColorValue& v) const
    {
        return ColorValue(r * v.r, g * v.g, b * v.b, a * v.a);
    }

    ColorValue operator*(float f) const
    {
        return ColorValue(r * f, g * f, b * f, a * f);
    }

    ColorValue& operator*=(float f)
    {
        r *= f;
        g *= f;
        b *= f;
        a *= f;

        return *this;
    }
};

} // namespace zem

#endif
