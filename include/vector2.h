#ifndef _ZEM_VECTOR2_H
#define _ZEM_VECTOR2_H

#include <cassert>
#include <cmath>
#include <cstddef>

namespace zem {

struct Vector2 {
public:
    float x, y;

    Vector2(float x = 0.0, float y = 0.0) : x(x), y(y) {}

    Vector2(const float coords[2]) : x(coords[0]), y(coords[1]) {}

    inline float operator[](size_t i) const
    {
        assert(i < 2);
        return *(&x + i);
    }

    inline float& operator[](size_t i)
    {
        assert(i < 2);
        return *(&x + i);
    }

    inline float* ptr() { return &x; }

    inline const float* ptr() const { return &x; }

    Vector2 operator+(const Vector2& v) const
    {
        return Vector2(x + v.x, y + v.y);
    }

    Vector2& operator+=(const Vector2& v)
    {
        x += v.x;
        y += v.y;

        return *this;
    }

    Vector2 operator-(const Vector2& v) const
    {
        return Vector2(x - v.x, y - v.y);
    }

    Vector2 operator*(const Vector2& v) const
    {
        return Vector2(x * v.x, y * v.y);
    }

    Vector2 operator*(float f) const { return Vector2(x * f, y * f); }

    Vector2& operator*=(float f)
    {
        x *= f;
        y *= f;

        return *this;
    }

    float dot(const Vector2& v) const { return x * v.x + y * v.y; }

    float length() const
    {
        float s = x * x + y * y;
        return (float)sqrt(s);
    }

    void normalize()
    {
        float len = length();

        if (len != 0.0f) {
            float inv = 1.0f / len;
            x *= inv;
            y *= inv;
        }
    }

    static Vector2 lerp(const Vector2& l, const Vector2& r, float t)
    {
        return r * t + l * (1.0f - t);
    }
};

} // namespace zem

#endif
