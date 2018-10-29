/*
 * Copyright 2014 Google Inc.
 *
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

/* Generated by tools/bookmaker from include/core/SkTextBlob.h and docs/SkTextBlob_Reference.bmh
   on 2018-08-10 12:59:44. Additional documentation and examples can be found at:
   https://skia.org/user/api/SkTextBlob_Reference

   You may edit either file directly. Structural changes to public interfaces require
   editing both files. After editing docs/SkTextBlob_Reference.bmh, run:
       bookmaker -b docs -i include/core/SkTextBlob.h -p
   to create an updated version of this file.
 */

#ifndef SkTextBlob_DEFINED
#define SkTextBlob_DEFINED

#include "../private/SkTemplates.h"
#include "SkFont.h"
#include "SkPaint.h"
#include "SkString.h"
#include "SkRefCnt.h"

#include <atomic>

#define SK_SUPPORT_LEGACY_TEXTBLOBBUILD_WITH_PAINT

struct SkSerialProcs;
struct SkDeserialProcs;

/** \class SkTextBlob
    SkTextBlob combines multiple text runs into an immutable container. Each text
    run consists of glyphs, SkPaint, and position. Only parts of SkPaint related to
    fonts and text rendering are used by run.
*/
class SK_API SkTextBlob final : public SkNVRefCnt<SkTextBlob> {
public:

    /** Returns conservative bounding box. Uses SkPaint associated with each glyph to
        determine glyph bounds, and unions all bounds. Returned bounds may be
        larger than the bounds of all glyphs in runs.

        @return  conservative bounding box
    */
    const SkRect& bounds() const { return fBounds; }

    /** Returns a non-zero value unique among all text blobs.

        @return  identifier for SkTextBlob
    */
    uint32_t uniqueID() const { return fUniqueID; }

    /** Creates SkTextBlob with a single run.

        font contains attributes used to define the run text:
        SkTypeface, SkPaint text size, SkPaint text scale x,
        SkFont text skew x, SkPaint::Align, SkFont::Hinting, anti-alias, SkFont fake bold,
        SkFont font embedded bitmaps, SkFont full hinting spacing, LCD text, SkFont linear text,
        and SkFont subpixel text.

        @param text        character code points or glyphs drawn
        @param byteLength  byte length of text array
        @param font       text size, typeface, text scale, and so on, used to draw
        @return            SkTextBlob constructed from one run
    */
    static sk_sp<SkTextBlob> MakeFromText(const void* text, size_t byteLength, const SkFont& font,
                                          SkPaint::TextEncoding = SkPaint::kUTF8_TextEncoding);

    /** Creates SkTextBlob with a single run. string meaning depends on SkPaint::TextEncoding;
        by default, string is encoded as UTF-8.

        font contains attributes used to define the run text:
        SkTypeface, SkPaint text size, SkPaint text scale x,
        SkFont text skew x, SkPaint::Align, SkFont::Hinting, anti-alias, SkFont fake bold,
        SkFont font embedded bitmaps, SkFont full hinting spacing, LCD text, SkFont linear text,
        and SkFont subpixel text.

        @param string  character code points or glyphs drawn
        @param font   text size, typeface, text scale, and so on, used to draw
        @return        SkTextBlob constructed from one run
    */
    static sk_sp<SkTextBlob> MakeFromString(const char* string, const SkFont& font,
                                    SkPaint::TextEncoding encoding = SkPaint::kUTF8_TextEncoding) {
        if (!string) {
            return nullptr;
        }
        return MakeFromText(string, strlen(string), font, encoding);
    }

    /** Writes data to allow later reconstruction of SkTextBlob. memory points to storage
        to receive the encoded data, and memory_size describes the size of storage.
        Returns bytes used if provided storage is large enough to hold all data;
        otherwise, returns zero.

        procs.fTypefaceProc permits supplying a custom function to encode SkTypeface.
        If procs.fTypefaceProc is nullptr, default encoding is used. procs.fTypefaceCtx
        may be used to provide user context to procs.fTypefaceProc; procs.fTypefaceProc
        is called with a pointer to SkTypeface and user context.

        @param procs   custom serial data encoders; may be nullptr
        @param memory  storage for data
        @param size    size of storage
        @return        bytes written, or zero if required storage is larger than memory_size
    */
    size_t serialize(const SkSerialProcs& procs, void* memory, size_t memory_size) const;

    /** Returns storage containing SkData describing SkTextBlob, using optional custom
        encoders.

        procs.fTypefaceProc permits supplying a custom function to encode SkTypeface.
        If procs.fTypefaceProc is nullptr, default encoding is used. procs.fTypefaceCtx
        may be used to provide user context to procs.fTypefaceProc; procs.fTypefaceProc
        is called with a pointer to SkTypeface and user context.

        @param procs  custom serial data encoders; may be nullptr
        @return       storage containing serialized SkTextBlob
    */
    sk_sp<SkData> serialize(const SkSerialProcs& procs) const;

    /** Recreates SkTextBlob that was serialized into data. Returns constructed SkTextBlob
        if successful; otherwise, returns nullptr. Fails if size is smaller than
        required data length, or if data does not permit constructing valid SkTextBlob.

        procs.fTypefaceProc permits supplying a custom function to decode SkTypeface.
        If procs.fTypefaceProc is nullptr, default decoding is used. procs.fTypefaceCtx
        may be used to provide user context to procs.fTypefaceProc; procs.fTypefaceProc
        is called with a pointer to SkTypeface data, data byte length, and user context.

        @param data   pointer for serial data
        @param size   size of data
        @param procs  custom serial data decoders; may be nullptr
        @return       SkTextBlob constructed from data in memory
    */
    static sk_sp<SkTextBlob> Deserialize(const void* data, size_t size,
                                         const SkDeserialProcs& procs);

private:
    friend class SkNVRefCnt<SkTextBlob>;
    class RunRecord;

    enum GlyphPositioning : uint8_t;

    explicit SkTextBlob(const SkRect& bounds);

    ~SkTextBlob();

    // Memory for objects of this class is created with sk_malloc rather than operator new and must
    // be freed with sk_free.
    void operator delete(void* p);
    void* operator new(size_t);
    void* operator new(size_t, void* p);

    static unsigned ScalarsPerGlyph(GlyphPositioning pos);

    // Call when this blob is part of the key to a cache entry. This allows the cache
    // to know automatically those entries can be purged when this SkTextBlob is deleted.
    void notifyAddedToCache(uint32_t cacheID) const {
        fCacheID.store(cacheID);
    }

    friend class SkGlyphRunList;
    friend class GrTextBlobCache;
    friend class SkTextBlobBuilder;
    friend class SkTextBlobPriv;
    friend class SkTextBlobRunIterator;

    const SkRect                  fBounds;
    const uint32_t                fUniqueID;
    mutable std::atomic<uint32_t> fCacheID;

    SkDEBUGCODE(size_t fStorageSize;)

    // The actual payload resides in externally-managed storage, following the object.
    // (see the .cpp for more details)

    typedef SkRefCnt INHERITED;
};

/** \class SkTextBlobBuilder
    Helper class for constructing SkTextBlob.
*/
class SK_API SkTextBlobBuilder {
public:

    /** Constructs empty SkTextBlobBuilder. By default, SkTextBlobBuilder has no runs.

        @return  empty SkTextBlobBuilder
    */
    SkTextBlobBuilder();

    /** Deletes data allocated internally by SkTextBlobBuilder.
    */
    ~SkTextBlobBuilder();

    /** Returns SkTextBlob built from runs of glyphs added by builder. Returned
        SkTextBlob is immutable; it may be copied, but its contents may not be altered.
        Returns nullptr if no runs of glyphs were added by builder.

        Resets SkTextBlobBuilder to its initial empty state, allowing it to be
        reused to build a new set of runs.

        @return  SkTextBlob or nullptr
    */
    sk_sp<SkTextBlob> make();

    /** \struct SkTextBlobBuilder::RunBuffer
        RunBuffer supplies storage for glyphs and positions within a run.

        A run is a sequence of glyphs sharing SkPaint::FontMetrics and positioning.
        Each run may position its glyphs in one of three ways:
        by specifying where the first glyph is drawn, and allowing SkPaint::FontMetrics to
        determine the advance to subsequent glyphs; by specifying a baseline, and
        the position on that baseline for each glyph in run; or by providing SkPoint
        array, one per glyph.
    */
    struct RunBuffer {
        SkGlyphID* glyphs;   //!< storage for glyphs in run
        SkScalar*  pos;      //!< storage for positions in run
        char*      utf8text; //!< reserved for future use
        uint32_t*  clusters; //!< reserved for future use
    };

#ifdef SK_SUPPORT_LEGACY_TEXTBLOBBUILD_WITH_PAINT
    /** Returns run with storage for glyphs. Caller must write count glyphs to
        RunBuffer.glyphs() before next call to FontBlobBuilder.

        RunBuffer.utf8text(), and RunBuffer.clusters() should be ignored.

        Glyphs share SkPaint::FontMetrics in font, including:
        SkTypeface, SkPaint text size, SkPaint text scale x,
        SkPaint text skew x, SkPaint::Align, SkPaint::Hinting, anti-alias, SkPaint fake bold,
        SkPaint font embedded bitmaps, SkPaint full hinting spacing, LCD text, SkPaint linear text,
        and SkPaint subpixel text.

        Glyphs are positioned on a baseline at (x, y), using font SkPaint::FontMetrics to
        determine their relative placement.

        bounds defines an optional bounding box, used to suppress drawing when SkTextBlob
        bounds does not intersect SkSurface bounds. If bounds is nullptr, SkTextBlob bounds
        is computed from (x, y) and RunBuffer.glyphs() SkPaint::FontMetrics.

        @param font    SkPaint used for this run
        @param count   number of glyphs
        @param x       horizontal offset within the blob
        @param y       vertical offset within the blob
        @param bounds  optional run bounding box
        @return        writable glyph buffer
    */
    const RunBuffer& allocRun(const SkPaint& font, int count, SkScalar x, SkScalar y,
                              const SkRect* bounds = nullptr) {
        return this->allocRunText(font, count, x, y, 0, SkString(), bounds);
    }

    /** Returns run with storage for glyphs and positions along baseline. Caller must
        write count glyphs to RunBuffer.glyphs(), and count scalars to RunBuffer.pos();
        before next call to FontBlobBuilder.

        RunBuffer.utf8text(), and RunBuffer.clusters() should be ignored.

        Glyphs share SkPaint::FontMetrics in font, including:
        SkTypeface, SkPaint text size, SkPaint text scale x,
        SkPaint text skew x, SkPaint::Align, SkPaint::Hinting, anti-alias, SkPaint fake bold,
        SkPaint font embedded bitmaps, SkPaint full hinting spacing, LCD text, SkPaint linear text,
        and SkPaint subpixel text.

        Glyphs are positioned on a baseline at y, using x-axis positions written by
        caller to RunBuffer.pos().

        bounds defines an optional bounding box, used to suppress drawing when SkTextBlob
        bounds does not intersect SkSurface bounds. If bounds is nullptr, SkTextBlob bounds
        is computed from y, RunBuffer.pos(), and RunBuffer.glyphs() SkPaint::FontMetrics.

        @param font    SkPaint used for this run
        @param count   number of glyphs
        @param y       vertical offset within the blob
        @param bounds  optional run bounding box
        @return        writable glyph buffer and x-axis position buffer
    */
    const RunBuffer& allocRunPosH(const SkPaint& font, int count, SkScalar y,
                                  const SkRect* bounds = nullptr) {
        return this->allocRunTextPosH(font, count, y, 0, SkString(), bounds);
    }

    /** Returns run with storage for glyphs and SkPoint positions. Caller must
        write count glyphs to RunBuffer.glyphs(), and count SkPoint to RunBuffer.pos();
        before next call to FontBlobBuilder.

        RunBuffer.utf8text(), and RunBuffer.clusters() should be ignored.

        Glyphs share SkPaint::FontMetrics in font, including:
        SkTypeface, SkPaint text size, SkPaint text scale x,
        SkPaint text skew x, SkPaint::Align, SkPaint::Hinting, anti-alias, SkPaint fake bold,
        SkPaint font embedded bitmaps, SkPaint full hinting spacing, LCD text, SkPaint linear text,
        and SkPaint subpixel text.

        Glyphs are positioned using SkPoint written by caller to RunBuffer.pos(), using
        two scalar values for each SkPoint.

        bounds defines an optional bounding box, used to suppress drawing when SkTextBlob
        bounds does not intersect SkSurface bounds. If bounds is nullptr, SkTextBlob bounds
        is computed from RunBuffer.pos(), and RunBuffer.glyphs() SkPaint::FontMetrics.

        @param font    SkPaint used for this run
        @param count   number of glyphs
        @param bounds  optional run bounding box
        @return        writable glyph buffer and SkPoint buffer
    */
    const RunBuffer& allocRunPos(const SkPaint& font, int count,
                                 const SkRect* bounds = nullptr) {
        return this->allocRunTextPos(font, count, 0, SkString(), bounds);
    }
#endif

    const RunBuffer& allocRun(const SkFont& font, int count, SkScalar x, SkScalar y,
                              const SkRect* bounds = nullptr);
    const RunBuffer& allocRunPosH(const SkFont& font, int count, SkScalar y,
                                  const SkRect* bounds = nullptr);
    const RunBuffer& allocRunPos(const SkFont& font, int count,
                                 const SkRect* bounds = nullptr);

private:
    const RunBuffer& allocRunText(const SkPaint& font,
                                  int count,
                                  SkScalar x,
                                  SkScalar y,
                                  int textByteCount,
                                  SkString lang,
                                  const SkRect* bounds = nullptr);
    const RunBuffer& allocRunTextPosH(const SkPaint& font, int count, SkScalar y,
                                      int textByteCount, SkString lang,
                                      const SkRect* bounds = nullptr);
    const RunBuffer& allocRunTextPos(const SkPaint& font, int count,
                                     int textByteCount, SkString lang,
                                     const SkRect* bounds = nullptr);
    void reserve(size_t size);
    void allocInternal(const SkPaint& font, SkTextBlob::GlyphPositioning positioning,
                       int count, int textBytes, SkPoint offset, const SkRect* bounds);
    bool mergeRun(const SkPaint& font, SkTextBlob::GlyphPositioning positioning,
                  uint32_t count, SkPoint offset);
    void updateDeferredBounds();

    static SkRect ConservativeRunBounds(const SkTextBlob::RunRecord&);
    static SkRect TightRunBounds(const SkTextBlob::RunRecord&);

    friend class SkTextBlobPriv;
    friend class SkTextBlobBuilderPriv;

    SkAutoTMalloc<uint8_t> fStorage;
    size_t                 fStorageSize;
    size_t                 fStorageUsed;

    SkRect                 fBounds;
    int                    fRunCount;
    bool                   fDeferredBounds;
    size_t                 fLastRun; // index into fStorage

    RunBuffer              fCurrentRunBuffer;
};

#endif // SkTextBlob_DEFINED
