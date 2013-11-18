module UniformLinear where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Foreign.Ptr
import Foreign.Marshal.Utils
import Linear


uniformMatrix2f :: UniformLocation -> SettableStateVar (M22 GLfloat)
uniformMatrix2f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix2fv loc 1 1) . castPtr))

uniformMatrix3f :: UniformLocation -> SettableStateVar (M33 GLfloat)
uniformMatrix3f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix3fv loc 1 1) . castPtr))

uniformMatrix4f :: UniformLocation -> SettableStateVar (M44 GLfloat)
uniformMatrix4f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix4fv loc 1 1) . castPtr))

uniformMatrix2x3f :: UniformLocation -> SettableStateVar (V3 (V2 GLfloat))
uniformMatrix2x3f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix2x3fv loc 1 1) . castPtr))

uniformMatrix3x2f :: UniformLocation -> SettableStateVar (V2 (V3 GLfloat))
uniformMatrix3x2f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix3x2fv loc 1 1) . castPtr))

uniformMatrix2x4f :: UniformLocation -> SettableStateVar (V4 (V2 GLfloat))
uniformMatrix2x4f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix2x4fv loc 1 1) . castPtr))

uniformMatrix4x2f :: UniformLocation -> SettableStateVar (V2 (V4 GLfloat))
uniformMatrix4x2f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix4x2fv loc 1 1) . castPtr))

uniformMatrix3x4f :: UniformLocation -> SettableStateVar (V4 (V3 GLfloat))
uniformMatrix3x4f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix3x4fv loc 1 1) . castPtr))

uniformMatrix4x3f :: UniformLocation -> SettableStateVar (V3 (V4 GLfloat))
uniformMatrix4x3f (UniformLocation loc) = makeSettableStateVar (\mat -> with mat ((glUniformMatrix4x3fv loc 1 1) . castPtr))
