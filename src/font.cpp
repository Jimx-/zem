#include "font.h"

#include <glad/glad.h>

#include "spdlog/spdlog.h"
#include <stdexcept>

namespace zem {

struct FTLibrary {
    FT_Library ft;

    FTLibrary() { FT_Init_FreeType(&ft); }
    ~FTLibrary() { FT_Done_FreeType(ft); }
};

static thread_local FTLibrary g_ft;

Font::Font(const std::string& filename, unsigned int size_pixels)
    : filename(filename), size_pixels(size_pixels)
{
    if (FT_New_Face(g_ft.ft, filename.c_str(), 0, &face)) {
        spdlog::error("Failed to load font '{}'", filename);
        return;
    }

    FT_Select_Charmap(face, FT_ENCODING_UNICODE);

    FT_Set_Pixel_Sizes(face, 0, size_pixels);
}

Font::~Font() { FT_Done_Face(face); }

unsigned int Font::get_height_pixels() const
{
    return face->size->metrics.height >> 6;
}

std::tuple<const GlyphInfo&, unsigned int> Font::find_glyph(uint32_t codepoint)
{
    unsigned atlas_idx = codepoint >> 8;

    auto iter = atlas_set.find(atlas_idx);
    if (iter == atlas_set.end()) {
        FontAtlas new_atlas;
        load_atlas(atlas_idx, new_atlas);
        iter = atlas_set.insert({atlas_idx, new_atlas}).first;
    }

    auto& atlas = iter->second;

    return std::tie(atlas.glyphs[codepoint & 0xff], atlas.tex_id);
}

void Font::load_atlas(unsigned int idx, FontAtlas& atlas)
{
    std::unique_ptr<unsigned char[]> buf;

    unsigned int max_dim = (1 + (face->size->metrics.height >> 6)) *
                           ceilf(sqrtf(FontAtlas::GLYPH_COUNT));
    unsigned int tex_width = 1;
    while (tex_width < max_dim)
        tex_width <<= 1;
    unsigned int tex_height = tex_width;

    buf = std::make_unique<unsigned char[]>(tex_width * tex_height * 4);
    unsigned int u, v;
    u = v = 0;

    for (int i = 0; i < FontAtlas::GLYPH_COUNT; i++) {
        unsigned long glyph_idx = FT_Get_Char_Index(face, (idx << 8) | i);
        if (!glyph_idx) continue;

        if (FT_Load_Glyph(face, glyph_idx, 0)) continue;
        if (FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL)) continue;

        FT_Bitmap& bmp = face->glyph->bitmap;

        if (u + bmp.width >= tex_width) {
            u = 0;
            v += ((face->size->metrics.height >> 6) + 1);
        }

        for (int row = 0; row < bmp.rows; ++row) {
            for (int col = 0; col < bmp.width; ++col) {
                int x = u + col;
                int y = v + row;

                buf[((y * tex_width + x) << 2) + 0] = 255;
                buf[((y * tex_width + x) << 2) + 1] = 255;
                buf[((y * tex_width + x) << 2) + 2] = 255;
                buf[((y * tex_width + x) << 2) + 3] =
                    bmp.buffer[row * bmp.pitch + col];
            }
        }

        auto& glyph = atlas.glyphs[i];

        glyph.x0 = face->glyph->bitmap_left;
        glyph.y0 = -face->glyph->bitmap_top;
        glyph.x1 = glyph.x0 + bmp.width;
        glyph.y1 = glyph.y0 + bmp.rows;

        glyph.u0 = u / (float)tex_width;
        glyph.v0 = v / (float)tex_height;
        glyph.u1 = (u + bmp.width) / (float)tex_width;
        glyph.v1 = (v + bmp.rows) / (float)tex_height;

        glyph.x_advance = face->glyph->advance.x >> 6;

        u += bmp.width + 1;
    }

    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_width, tex_height, 0, GL_RGBA,
                 GL_UNSIGNED_BYTE, buf.get());
    glBindTexture(GL_TEXTURE_2D, 0);

    atlas.tex_id = texture;
    atlas.tex_width = tex_width;
    atlas.tex_height = tex_height;
}

} // namespace zem
