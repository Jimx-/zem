#ifndef _ZEM_FONT_H
#define _ZEM_FONT_H

#include <ft2build.h>
#include FT_FREETYPE_H

#include <string>
#include <unordered_map>

namespace zem {

struct GlyphInfo {
    float x0, y0, x1, y1;
    float u0, v0, u1, v1;
    float x_advance;
};

struct FontAtlas {
    unsigned int tex_id;
    unsigned int tex_width, tex_height;

    static const int GLYPH_COUNT = 256;
    GlyphInfo glyphs[GLYPH_COUNT];

    FontAtlas() { memset(this, 0, sizeof(*this)); }
};

class Font {
public:
    Font(const std::string& filename, unsigned int size_pixels);
    ~Font();

    unsigned int get_height_pixels() const;

    std::tuple<const GlyphInfo&, unsigned int> find_glyph(uint32_t codepoint);

private:
    std::string filename;
    unsigned int size_pixels;
    FT_Face face;
    std::unordered_map<unsigned int, FontAtlas> atlas_set;

    void load_atlas(unsigned int idx, FontAtlas& atlas);
};

} // namespace zem

#endif
