#include <mapbox/glyph_foundry.hpp>

extern "C" {

void hs_mapbox_sdf_destroyGlyphInfo(void *p) {
  delete static_cast<sdf_glyph_foundry::glyph_info*>(p);
}

}

