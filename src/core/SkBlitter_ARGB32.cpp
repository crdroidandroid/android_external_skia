/*
 * Copyright 2006 The Android Open Source Project
 *
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#include "SkCoreBlitters.h"
#include "SkColorPriv.h"
#include "SkShader.h"
#include "SkUtils.h"
#include "SkXfermodePriv.h"
#include "SkBlitMask.h"

#include <semaphore.h>
#include <assert.h>
#include <pthread.h>
#include <cutils/log.h>
///////////////////////////////////////////////////////////////////////////////

static void SkARGB32_Blit32(const SkPixmap& device, const SkMask& mask,
                            const SkIRect& clip, SkPMColor srcColor) {
    U8CPU alpha = SkGetPackedA32(srcColor);
    unsigned flags = SkBlitRow::kSrcPixelAlpha_Flag32;
    if (alpha != 255) {
        flags |= SkBlitRow::kGlobalAlpha_Flag32;
    }
    SkBlitRow::Proc32 proc = SkBlitRow::Factory32(flags);

    int x = clip.fLeft;
    int y = clip.fTop;
    int width = clip.width();
    int height = clip.height();

    SkPMColor* dstRow = device.writable_addr32(x, y);
    const SkPMColor* srcRow = reinterpret_cast<const SkPMColor*>(mask.getAddr8(x, y));

    do {
        proc(dstRow, srcRow, width, alpha);
        dstRow = (SkPMColor*)((char*)dstRow + device.rowBytes());
        srcRow = (const SkPMColor*)((const char*)srcRow + mask.fRowBytes);
    } while (--height != 0);
}

//////////////////////////////////////////////////////////////////////////////////////

SkARGB32_Blitter::SkARGB32_Blitter(const SkPixmap& device, const SkPaint& paint)
        : INHERITED(device) {
    SkColor color = paint.getColor();
    fColor = color;

    fSrcA = SkColorGetA(color);
    unsigned scale = SkAlpha255To256(fSrcA);
    fSrcR = SkAlphaMul(SkColorGetR(color), scale);
    fSrcG = SkAlphaMul(SkColorGetG(color), scale);
    fSrcB = SkAlphaMul(SkColorGetB(color), scale);

    fPMColor = SkPackARGB32(fSrcA, fSrcR, fSrcG, fSrcB);
}

const SkPixmap* SkARGB32_Blitter::justAnOpaqueColor(uint32_t* value) {
    if (255 == fSrcA) {
        *value = fPMColor;
        return &fDevice;
    }
    return nullptr;
}

#if defined _WIN32  // disable warning : local variable used without having been initialized
#pragma warning ( push )
#pragma warning ( disable : 4701 )
#endif

void SkARGB32_Blitter::blitH(int x, int y, int width) {
    SkASSERT(x >= 0 && y >= 0 && x + width <= fDevice.width());

    uint32_t* device = fDevice.writable_addr32(x, y);
    SkBlitRow::Color32(device, device, width, fPMColor);
}

void SkARGB32_Blitter::blitAntiH(int x, int y, const SkAlpha antialias[],
                                 const int16_t runs[]) {
    if (fSrcA == 0) {
        return;
    }

    uint32_t    color = fPMColor;
    uint32_t*   device = fDevice.writable_addr32(x, y);
    unsigned    opaqueMask = fSrcA; // if fSrcA is 0xFF, then we will catch the fast opaque case

    for (;;) {
        int count = runs[0];
        SkASSERT(count >= 0);
        if (count <= 0) {
            return;
        }
        unsigned aa = antialias[0];
        if (aa) {
            if ((opaqueMask & aa) == 255) {
                sk_memset32(device, color, count);
            } else {
                uint32_t sc = SkAlphaMulQ(color, SkAlpha255To256(aa));
                SkBlitRow::Color32(device, device, count, sc);
            }
        }
        runs += count;
        antialias += count;
        device += count;
    }
}

void SkARGB32_Blitter::blitAntiH2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x + 1, y);)

    device[0] = SkBlendARGB32(fPMColor, device[0], a0);
    device[1] = SkBlendARGB32(fPMColor, device[1], a1);
}

void SkARGB32_Blitter::blitAntiV2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x, y + 1);)

    device[0] = SkBlendARGB32(fPMColor, device[0], a0);
    device = (uint32_t*)((char*)device + fDevice.rowBytes());
    device[0] = SkBlendARGB32(fPMColor, device[0], a1);
}

//////////////////////////////////////////////////////////////////////////////////////

#define solid_8_pixels(mask, dst, color)    \
    do {                                    \
        if (mask & 0x80) dst[0] = color;    \
        if (mask & 0x40) dst[1] = color;    \
        if (mask & 0x20) dst[2] = color;    \
        if (mask & 0x10) dst[3] = color;    \
        if (mask & 0x08) dst[4] = color;    \
        if (mask & 0x04) dst[5] = color;    \
        if (mask & 0x02) dst[6] = color;    \
        if (mask & 0x01) dst[7] = color;    \
    } while (0)

#define SK_BLITBWMASK_NAME                  SkARGB32_BlitBW
#define SK_BLITBWMASK_ARGS                  , SkPMColor color
#define SK_BLITBWMASK_BLIT8(mask, dst)      solid_8_pixels(mask, dst, color)
#define SK_BLITBWMASK_GETADDR               writable_addr32
#define SK_BLITBWMASK_DEVTYPE               uint32_t
#include "SkBlitBWMaskTemplate.h"

#define blend_8_pixels(mask, dst, sc, dst_scale)                            \
    do {                                                                    \
        if (mask & 0x80) { dst[0] = sc + SkAlphaMulQ(dst[0], dst_scale); }  \
        if (mask & 0x40) { dst[1] = sc + SkAlphaMulQ(dst[1], dst_scale); }  \
        if (mask & 0x20) { dst[2] = sc + SkAlphaMulQ(dst[2], dst_scale); }  \
        if (mask & 0x10) { dst[3] = sc + SkAlphaMulQ(dst[3], dst_scale); }  \
        if (mask & 0x08) { dst[4] = sc + SkAlphaMulQ(dst[4], dst_scale); }  \
        if (mask & 0x04) { dst[5] = sc + SkAlphaMulQ(dst[5], dst_scale); }  \
        if (mask & 0x02) { dst[6] = sc + SkAlphaMulQ(dst[6], dst_scale); }  \
        if (mask & 0x01) { dst[7] = sc + SkAlphaMulQ(dst[7], dst_scale); }  \
    } while (0)

#define SK_BLITBWMASK_NAME                  SkARGB32_BlendBW
#define SK_BLITBWMASK_ARGS                  , uint32_t sc, unsigned dst_scale
#define SK_BLITBWMASK_BLIT8(mask, dst)      blend_8_pixels(mask, dst, sc, dst_scale)
#define SK_BLITBWMASK_GETADDR               writable_addr32
#define SK_BLITBWMASK_DEVTYPE               uint32_t
#include "SkBlitBWMaskTemplate.h"

void SkARGB32_Blitter::blitMask(const SkMask& mask, const SkIRect& clip) {
    SkASSERT(mask.fBounds.contains(clip));
    SkASSERT(fSrcA != 0xFF);

    if (fSrcA == 0) {
        return;
    }

    if (SkBlitMask::BlitColor(fDevice, mask, clip, fColor)) {
        return;
    }

    switch (mask.fFormat) {
        case SkMask::kBW_Format:
            SkARGB32_BlendBW(fDevice, mask, clip, fPMColor, SkAlpha255To256(255 - fSrcA));
            break;
        case SkMask::kARGB32_Format:
            SkARGB32_Blit32(fDevice, mask, clip, fPMColor);
            break;
        default:
            SkFAIL("Mask format not handled.");
    }
}

void SkARGB32_Opaque_Blitter::blitMask(const SkMask& mask,
                                       const SkIRect& clip) {
    SkASSERT(mask.fBounds.contains(clip));

    if (SkBlitMask::BlitColor(fDevice, mask, clip, fColor)) {
        return;
    }

    switch (mask.fFormat) {
        case SkMask::kBW_Format:
            SkARGB32_BlitBW(fDevice, mask, clip, fPMColor);
            break;
        case SkMask::kARGB32_Format:
            SkARGB32_Blit32(fDevice, mask, clip, fPMColor);
            break;
        default:
            SkFAIL("Mask format not handled.");
    }
}

void SkARGB32_Opaque_Blitter::blitAntiH2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x + 1, y);)

    device[0] = SkFastFourByteInterp(fPMColor, device[0], a0);
    device[1] = SkFastFourByteInterp(fPMColor, device[1], a1);
}

void SkARGB32_Opaque_Blitter::blitAntiV2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x, y + 1);)

    device[0] = SkFastFourByteInterp(fPMColor, device[0], a0);
    device = (uint32_t*)((char*)device + fDevice.rowBytes());
    device[0] = SkFastFourByteInterp(fPMColor, device[0], a1);
}

///////////////////////////////////////////////////////////////////////////////

void SkARGB32_Blitter::blitV(int x, int y, int height, SkAlpha alpha) {
    if (alpha == 0 || fSrcA == 0) {
        return;
    }

    uint32_t* device = fDevice.writable_addr32(x, y);
    uint32_t  color = fPMColor;

    if (alpha != 255) {
        color = SkAlphaMulQ(color, SkAlpha255To256(alpha));
    }

    unsigned dst_scale = SkAlpha255To256(255 - SkGetPackedA32(color));
    size_t rowBytes = fDevice.rowBytes();
    while (--height >= 0) {
        device[0] = color + SkAlphaMulQ(device[0], dst_scale);
        device = (uint32_t*)((char*)device + rowBytes);
    }
}

void SkARGB32_Blitter::blitRect(int x, int y, int width, int height) {
    SkASSERT(x >= 0 && y >= 0 && x + width <= fDevice.width() && y + height <= fDevice.height());

    if (fSrcA == 0) {
        return;
    }

    uint32_t*   device = fDevice.writable_addr32(x, y);
    uint32_t    color = fPMColor;
    size_t      rowBytes = fDevice.rowBytes();

    while (--height >= 0) {
        SkBlitRow::Color32(device, device, width, color);
        device = (uint32_t*)((char*)device + rowBytes);
    }
}

#if defined _WIN32
#pragma warning ( pop )
#endif

///////////////////////////////////////////////////////////////////////

void SkARGB32_Black_Blitter::blitAntiH(int x, int y, const SkAlpha antialias[],
                                       const int16_t runs[]) {
    uint32_t*   device = fDevice.writable_addr32(x, y);
    SkPMColor   black = (SkPMColor)(SK_A32_MASK << SK_A32_SHIFT);

    for (;;) {
        int count = runs[0];
        SkASSERT(count >= 0);
        if (count <= 0) {
            return;
        }
        unsigned aa = antialias[0];
        if (aa) {
            if (aa == 255) {
                sk_memset32(device, black, count);
            } else {
                SkPMColor src = aa << SK_A32_SHIFT;
                unsigned dst_scale = 256 - aa;
                int n = count;
                do {
                    --n;
                    device[n] = src + SkAlphaMulQ(device[n], dst_scale);
                } while (n > 0);
            }
        }
        runs += count;
        antialias += count;
        device += count;
    }
}

void SkARGB32_Black_Blitter::blitAntiH2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x + 1, y);)

    device[0] = (a0 << SK_A32_SHIFT) + SkAlphaMulQ(device[0], 256 - a0);
    device[1] = (a1 << SK_A32_SHIFT) + SkAlphaMulQ(device[1], 256 - a1);
}

void SkARGB32_Black_Blitter::blitAntiV2(int x, int y, U8CPU a0, U8CPU a1) {
    uint32_t* device = fDevice.writable_addr32(x, y);
    SkDEBUGCODE((void)fDevice.writable_addr32(x, y + 1);)

    device[0] = (a0 << SK_A32_SHIFT) + SkAlphaMulQ(device[0], 256 - a0);
    device = (uint32_t*)((char*)device + fDevice.rowBytes());
    device[0] = (a1 << SK_A32_SHIFT) + SkAlphaMulQ(device[0], 256 - a1);
}

///////////////////////////////////////////////////////////////////////////////

// Special version of SkBlitRow::Factory32 that knows we're in kSrc_Mode,
// instead of kSrcOver_Mode
static void blend_srcmode(SkPMColor* SK_RESTRICT device,
                          const SkPMColor* SK_RESTRICT span,
                          int count, U8CPU aa) {
    int aa256 = SkAlpha255To256(aa);
    for (int i = 0; i < count; ++i) {
        device[i] = SkFourByteInterp256(span[i], device[i], aa256);
    }
}

SkARGB32_Shader_Blitter::SkARGB32_Shader_Blitter(const SkPixmap& device,
        const SkPaint& paint, SkShaderBase::Context* shaderContext)
    : INHERITED(device, paint, shaderContext)
{
    fBuffer = (SkPMColor*)sk_malloc_throw(device.width() * (sizeof(SkPMColor)));

    fXfermode = SkXfermode::Peek(paint.getBlendMode());

    int flags = 0;
    if (!(shaderContext->getFlags() & SkShaderBase::kOpaqueAlpha_Flag)) {
        flags |= SkBlitRow::kSrcPixelAlpha_Flag32;
    }
    // we call this on the output from the shader
    fProc32 = SkBlitRow::Factory32(flags);
    // we call this on the output from the shader + alpha from the aa buffer
    fProc32Blend = SkBlitRow::Factory32(flags | SkBlitRow::kGlobalAlpha_Flag32);

    fShadeDirectlyIntoDevice = false;
    if (fXfermode == nullptr) {
        if (shaderContext->getFlags() & SkShaderBase::kOpaqueAlpha_Flag) {
            fShadeDirectlyIntoDevice = true;
        }
    } else {
        if (SkBlendMode::kSrc == paint.getBlendMode()) {
            fShadeDirectlyIntoDevice = true;
            fProc32Blend = blend_srcmode;
        }
    }

    fConstInY = SkToBool(shaderContext->getFlags() & SkShaderBase::kConstInY32_Flag);
}

SkARGB32_Shader_Blitter::~SkARGB32_Shader_Blitter() {
    sk_free(fBuffer);
}

void SkARGB32_Shader_Blitter::blitH(int x, int y, int width) {
    SkASSERT(x >= 0 && y >= 0 && x + width <= fDevice.width());

    uint32_t* device = fDevice.writable_addr32(x, y);

    if (fShadeDirectlyIntoDevice) {
        fShaderContext->shadeSpan(x, y, device, width);
    } else {
        SkPMColor*  span = fBuffer;
        fShaderContext->shadeSpan(x, y, span, width);
        if (fXfermode) {
            fXfermode->xfer32(device, span, width, nullptr);
        } else {
            fProc32(device, span, width, 255);
        }
    }
}

#define BUF_MAX 8192
#define __ATOMIC_INLINE__ static __inline__ __attribute__((always_inline))

__ATOMIC_INLINE__ int __atomic_dec(volatile int *ptr) {
  return __sync_fetch_and_sub (ptr, 1);
}

__ATOMIC_INLINE__ int __atomic_inc(volatile int *ptr) {
  return __sync_fetch_and_add (ptr, 1);
}

static volatile int worker_thread_inited=0;
static volatile int worker_thread_busy=0;
static volatile void *work=0;

#define WORKER_THREADS_NUM 1

static pthread_t worker_threads[WORKER_THREADS_NUM];
sem_t sem_todo[WORKER_THREADS_NUM];
sem_t sem_done[WORKER_THREADS_NUM];

extern const char* __progname;
static int is_not_zygote(){

    const char * buf = __progname;
    const char * zygote = "zygote";
    /*
     * adb shell cat /proc/192/cmdline
     * zygote/bin/app_process-Xzygote/system/bin--zygote--start-system-server
     */
    return strncmp( buf, zygote, 6);
}

struct shadeSpanArg {
    SkShaderBase::Context* shaderContext;
    int x;
    int y;
    int width;
    int height;
    uint32_t* device;
    size_t deviceRB;
    bool fShadeDirectlyIntoDevice ;
    bool fConstInY;
    SkXfermode* fXfermode ;
    SkBlitRow::Proc32 fProc32 ;
    SkPMColor* fBuffer;
};


void blitRect_core(
        SkShaderBase::Context* shaderContext,
        int x,
        int y,
        int width,
        int height,
        uint32_t* device,
        size_t deviceRB,
        bool fShadeDirectlyIntoDevice,
        bool fConstInY,
        SkXfermode* fXfermode,
        SkBlitRow::Proc32 fProc32,
        SkPMColor* fbuffer)
{
    SkPMColor*  span = NULL;
    if (fbuffer) {
       span = fbuffer;
    } else {
       SkASSERT(width <= BUF_MAX);
       uint32_t buffer[BUF_MAX];
       span = (SkPMColor*)buffer;
    }

    if (fConstInY) {
        if (fShadeDirectlyIntoDevice) {
            // shade the first row directly into the device
            shaderContext->shadeSpan(x, y, device, width);
            span = device;
            while (--height > 0) {
                device = (uint32_t*)((char*)device + deviceRB);
                memcpy(device, span, width << 2);
            }
        } else {
            shaderContext->shadeSpan(x, y, span, width);
            SkXfermode* xfer = fXfermode;
            if (xfer) {
                do {
                    xfer->xfer32(device, span, width, nullptr);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            } else {
                SkBlitRow::Proc32 proc = fProc32;
                do {
                    proc(device, span, width, 255);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            }
        }
        return;
    }

    if (fShadeDirectlyIntoDevice) {
        void* ctx;
        auto shadeProc = shaderContext->asAShadeProc(&ctx);
        if (shadeProc) {
            do {
                shadeProc(ctx, x, y, device, width);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        } else {
            do {
                shaderContext->shadeSpan(x, y, device, width);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        }
    } else {
        SkXfermode* xfer = fXfermode;
        if (xfer) {
            do {
                shaderContext->shadeSpan(x, y, span, width);
                xfer->xfer32(device, span, width, nullptr);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        } else {
            SkBlitRow::Proc32 proc = fProc32;
            do {
                shaderContext->shadeSpan(x, y, span, width);
                proc(device, span, width, 255);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        }
    }
}

void * worker_blitRect(void * arg) {

    long id=(long)arg;

    //worker thread
    while(1){
        sem_wait( &sem_todo[id] );
        if ( work != 0 ) {
            struct shadeSpanArg * arg= (struct shadeSpanArg *)work;
            SkShaderBase::Context* shaderContext =arg->shaderContext;
            int x = arg->x;
            int y = arg->y;
            int width = arg->width;
            int height = arg->height;
            uint32_t* device = arg->device;
            size_t deviceRB = arg->deviceRB;

            bool fShadeDirectlyIntoDevice = arg->fShadeDirectlyIntoDevice;
            bool fConstInY = arg->fConstInY;
            SkXfermode* fXfermode = arg->fXfermode;
            SkBlitRow::Proc32 fProc32 = arg->fProc32;
            SkPMColor* fBuffer = arg->fBuffer;

            blitRect_core(
                    shaderContext,
                    x,
                    y,
                    width,
                    height,
                    device,
                    deviceRB,
                    fShadeDirectlyIntoDevice,
                    fConstInY,
                    fXfermode,
                    fProc32,
                    fBuffer
                    );

            work = 0;
        }
        sem_post( &sem_done[id] );
    }
    pthread_exit(0);
    return (void *)0;
}


void SkARGB32_Shader_Blitter::blitRect(int x, int y, int width, int height) {
    SkASSERT(x >= 0 && y >= 0 &&
             x + width <= fDevice.width() && y + height <= fDevice.height());

    uint32_t*  device = fDevice.writable_addr32(x, y);
    size_t     deviceRB = fDevice.rowBytes();
    auto*      shaderContext = fShaderContext;
    SkPMColor* span = fBuffer;

    if (fShaderContext->getID() != SkShaderBase::Context::kSkBitmapProcShader_Class) {
        goto fb;
    }

    if( ((height*width) > (750 * 400)) && (width <= BUF_MAX) ) {
        struct shadeSpanArg msg;
        if (__atomic_inc( &worker_thread_busy ) == 0) {
            if( !worker_thread_inited ) {
                if( __atomic_inc( &worker_thread_inited ) == 0 ) {
                    //Only start the other thread in non-zygote mode.
                    if( !is_not_zygote() ){
                        __atomic_dec( &worker_thread_busy );
                        __atomic_dec( &worker_thread_inited );
                        goto fb;
                    }
                    long i;
                    for( i = 0; i < WORKER_THREADS_NUM; i++ ){
                        sem_init(&sem_todo[i], 0, 0);
                        sem_init(&sem_done[i], 0, 0);
                        pthread_create( &worker_threads[i], NULL, worker_blitRect, (void *)i );
                    }

                }
            }
            msg.shaderContext = shaderContext;
            msg.x = x;
            msg.y = (y + (height>>1));
            msg.width = width;
            msg.height = (height - (height>>1));
            msg.device = (uint32_t*)((char*)device + (deviceRB * (height>>1)));
            msg.deviceRB = (deviceRB);

            msg.fShadeDirectlyIntoDevice = fShadeDirectlyIntoDevice;
            msg.fConstInY = fConstInY;
            msg.fXfermode = fXfermode;
            msg.fProc32 = fProc32;
            msg.fBuffer = NULL;

            /* Construct the input for worker_thread and start work on it
             * Currently there is only one worker_thread
             * If more worker threads are added, the worker thread input needs
             * to be constructed accordingly and different to_worker_thread used
             */

            int to_worker_thread;

            assert( work == 0 );
            work = &msg;

            to_worker_thread = 0;
            height = height >> 1;

            sem_post( &sem_todo[to_worker_thread] );

            blitRect_core(
                    shaderContext,
                    x,
                    y,
                    width,
                    height,
                    device,
                    deviceRB,
                    fShadeDirectlyIntoDevice,
                    fConstInY,
                    fXfermode,
                    fProc32,
                    NULL
                    );


            sem_wait( &sem_done[to_worker_thread] );

            /* Need to set to NULL value. No point in still storing addresso of local
             * variable, after function returns
             */
            work = NULL;

            __atomic_dec( &worker_thread_busy );
        }
        else {
            __atomic_dec( &worker_thread_busy );
            goto fb;
        }

        return;
    }


fb:
    blitRect_core(
            shaderContext,
            x,
            y,
            width,
            height,
            device,
            deviceRB,
            fShadeDirectlyIntoDevice,
            fConstInY,
            fXfermode,
            fProc32,
            fBuffer
            );
}

void SkARGB32_Shader_Blitter::blitAntiH(int x, int y, const SkAlpha antialias[],
                                        const int16_t runs[]) {
    SkPMColor* span = fBuffer;
    uint32_t*  device = fDevice.writable_addr32(x, y);
    auto*      shaderContext = fShaderContext;

    if (fXfermode && !fShadeDirectlyIntoDevice) {
        for (;;) {
            SkXfermode* xfer = fXfermode;

            int count = *runs;
            if (count <= 0)
                break;
            int aa = *antialias;
            if (aa) {
                shaderContext->shadeSpan(x, y, span, count);
                if (aa == 255) {
                    xfer->xfer32(device, span, count, nullptr);
                } else {
                    // count is almost always 1
                    for (int i = count - 1; i >= 0; --i) {
                        xfer->xfer32(&device[i], &span[i], 1, antialias);
                    }
                }
            }
            device += count;
            runs += count;
            antialias += count;
            x += count;
        }
    } else if (fShadeDirectlyIntoDevice ||
               (shaderContext->getFlags() & SkShaderBase::kOpaqueAlpha_Flag)) {
        for (;;) {
            int count = *runs;
            if (count <= 0) {
                break;
            }
            int aa = *antialias;
            if (aa) {
                if (aa == 255) {
                    // cool, have the shader draw right into the device
                    shaderContext->shadeSpan(x, y, device, count);
                } else {
                    shaderContext->shadeSpan(x, y, span, count);
                    fProc32Blend(device, span, count, aa);
                }
            }
            device += count;
            runs += count;
            antialias += count;
            x += count;
        }
    } else {
        for (;;) {
            int count = *runs;
            if (count <= 0) {
                break;
            }
            int aa = *antialias;
            if (aa) {
                shaderContext->shadeSpan(x, y, span, count);
                if (aa == 255) {
                    fProc32(device, span, count, 255);
                } else {
                    fProc32Blend(device, span, count, aa);
                }
            }
            device += count;
            runs += count;
            antialias += count;
            x += count;
        }
    }
}

void SkARGB32_Shader_Blitter::blitMask(const SkMask& mask, const SkIRect& clip) {
    // we only handle kA8 with an xfermode
    if (fXfermode && (SkMask::kA8_Format != mask.fFormat)) {
        this->INHERITED::blitMask(mask, clip);
        return;
    }

    SkASSERT(mask.fBounds.contains(clip));

    auto* shaderContext = fShaderContext;
    SkBlitMask::RowProc proc = nullptr;
    if (!fXfermode) {
        unsigned flags = 0;
        if (shaderContext->getFlags() & SkShaderBase::kOpaqueAlpha_Flag) {
            flags |= SkBlitMask::kSrcIsOpaque_RowFlag;
        }
        proc = SkBlitMask::RowFactory(kN32_SkColorType, mask.fFormat,
                                      (SkBlitMask::RowFlags)flags);
        if (nullptr == proc) {
            this->INHERITED::blitMask(mask, clip);
            return;
        }
    }

    const int x = clip.fLeft;
    const int width = clip.width();
    int y = clip.fTop;
    int height = clip.height();

    char* dstRow = (char*)fDevice.writable_addr32(x, y);
    const size_t dstRB = fDevice.rowBytes();
    const uint8_t* maskRow = (const uint8_t*)mask.getAddr(x, y);
    const size_t maskRB = mask.fRowBytes;

    SkPMColor* span = fBuffer;

    if (fXfermode) {
        SkASSERT(SkMask::kA8_Format == mask.fFormat);
        SkXfermode* xfer = fXfermode;
        do {
            shaderContext->shadeSpan(x, y, span, width);
            xfer->xfer32(reinterpret_cast<SkPMColor*>(dstRow), span, width, maskRow);
            dstRow += dstRB;
            maskRow += maskRB;
            y += 1;
        } while (--height > 0);
    } else {
        do {
            shaderContext->shadeSpan(x, y, span, width);
            proc(reinterpret_cast<SkPMColor*>(dstRow), maskRow, span, width);
            dstRow += dstRB;
            maskRow += maskRB;
            y += 1;
        } while (--height > 0);
    }
}

void SkARGB32_Shader_Blitter::blitV(int x, int y, int height, SkAlpha alpha) {
    SkASSERT(x >= 0 && y >= 0 && y + height <= fDevice.height());

    uint32_t* device = fDevice.writable_addr32(x, y);
    size_t    deviceRB = fDevice.rowBytes();
    auto*     shaderContext = fShaderContext;

    if (fConstInY) {
        SkPMColor c;
        shaderContext->shadeSpan(x, y, &c, 1);

        if (fShadeDirectlyIntoDevice) {
            if (255 == alpha) {
                do {
                    *device = c;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            } else {
                do {
                    *device = SkFourByteInterp(c, *device, alpha);
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            }
        } else {
            SkXfermode* xfer = fXfermode;
            if (xfer) {
                do {
                    xfer->xfer32(device, &c, 1, &alpha);
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            } else {
                SkBlitRow::Proc32 proc = (255 == alpha) ? fProc32 : fProc32Blend;
                do {
                    proc(device, &c, 1, alpha);
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            }
        }
        return;
    }

    if (fShadeDirectlyIntoDevice) {
        void* ctx;
        auto shadeProc = shaderContext->asAShadeProc(&ctx);
        if (255 == alpha) {
            if (shadeProc) {
                do {
                    shadeProc(ctx, x, y, device, 1);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            } else {
                do {
                    shaderContext->shadeSpan(x, y, device, 1);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            }
        } else {    // alpha < 255
            SkPMColor c;
            if (shadeProc) {
                do {
                    shadeProc(ctx, x, y, &c, 1);
                    *device = SkFourByteInterp(c, *device, alpha);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            } else {
                do {
                    shaderContext->shadeSpan(x, y, &c, 1);
                    *device = SkFourByteInterp(c, *device, alpha);
                    y += 1;
                    device = (uint32_t*)((char*)device + deviceRB);
                } while (--height > 0);
            }
        }
    } else {
        SkPMColor* span = fBuffer;
        SkXfermode* xfer = fXfermode;
        if (xfer) {
            do {
                shaderContext->shadeSpan(x, y, span, 1);
                xfer->xfer32(device, span, 1, &alpha);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        } else {
            SkBlitRow::Proc32 proc = (255 == alpha) ? fProc32 : fProc32Blend;
            do {
                shaderContext->shadeSpan(x, y, span, 1);
                proc(device, span, 1, alpha);
                y += 1;
                device = (uint32_t*)((char*)device + deviceRB);
            } while (--height > 0);
        }
    }
}
