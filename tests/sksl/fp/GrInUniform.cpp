

/**************************************************************************************************
 *** This file was autogenerated from GrInUniform.fp; do not modify.
 **************************************************************************************************/
#include "GrInUniform.h"

#include "src/core/SkUtils.h"
#include "src/gpu/GrTexture.h"
#include "src/gpu/glsl/GrGLSLFragmentProcessor.h"
#include "src/gpu/glsl/GrGLSLFragmentShaderBuilder.h"
#include "src/gpu/glsl/GrGLSLProgramBuilder.h"
#include "src/sksl/SkSLCPP.h"
#include "src/sksl/SkSLUtil.h"
class GrGLSLInUniform : public GrGLSLFragmentProcessor {
public:
    GrGLSLInUniform() {}
    void emitCode(EmitArgs& args) override {
        GrGLSLFPFragmentBuilder* fragBuilder = args.fFragBuilder;
        const GrInUniform& _outer = args.fFp.cast<GrInUniform>();
        (void) _outer;
        auto color = _outer.color;
        (void) color;
        colorVar = args.fUniformHandler->addUniform(&_outer, kFragment_GrShaderFlag, kHalf4_GrSLType, "color");
        fragBuilder->codeAppendf(
R"SkSL(return %s;
)SkSL"
, args.fUniformHandler->getUniformCStr(colorVar));
    }
private:
    void onSetData(const GrGLSLProgramDataManager& pdman, const GrFragmentProcessor& _proc) override {
        const GrInUniform& _outer = _proc.cast<GrInUniform>();
        {
        pdman.set4fv(colorVar, 1, reinterpret_cast<const float*>(&(_outer.color)));
        }
    }
    UniformHandle colorVar;
};
GrGLSLFragmentProcessor* GrInUniform::onCreateGLSLInstance() const {
    return new GrGLSLInUniform();
}
void GrInUniform::onGetGLSLProcessorKey(const GrShaderCaps& caps, GrProcessorKeyBuilder* b) const {
}
bool GrInUniform::onIsEqual(const GrFragmentProcessor& other) const {
    const GrInUniform& that = other.cast<GrInUniform>();
    (void) that;
    if (color != that.color) return false;
    return true;
}
GrInUniform::GrInUniform(const GrInUniform& src)
: INHERITED(kGrInUniform_ClassID, src.optimizationFlags())
, color(src.color) {
        this->cloneAndRegisterAllChildProcessors(src);
}
std::unique_ptr<GrFragmentProcessor> GrInUniform::clone() const {
    return std::make_unique<GrInUniform>(*this);
}
#if GR_TEST_UTILS
SkString GrInUniform::onDumpInfo() const {
    return SkStringPrintf("(color=half4(%f, %f, %f, %f))", color.left(), color.top(), color.right(), color.bottom());
}
#endif