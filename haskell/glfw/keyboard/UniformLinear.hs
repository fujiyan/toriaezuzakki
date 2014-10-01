-- | Provides uniformMatrix functions with Linear.
module UniformLinear
    ( uniformMatrix2fv
    , uniformMatrix3fv
    , uniformMatrix4fv
    , uniformMatrix2x3fv
    , uniformMatrix3x2fv
    , uniformMatrix2x4fv
    , uniformMatrix4x2fv
    , uniformMatrix3x4fv
    , uniformMatrix4x3fv
    ) where


import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Linear


-- | Returns the length of a list as a value of GLsizei.
len :: [a] -> GLsizei
len = fromIntegral . length


-- | Specifies the value of a uniform variable which is an array of 2 rows and 2 columns matrix for the current program object.
uniformMatrix2fv :: UniformLocation -> SettableStateVar [M22 GLfloat]
uniformMatrix2fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix2fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 3 rows and 3 columns matrix for the current program object.
uniformMatrix3fv :: UniformLocation -> SettableStateVar [M33 GLfloat]
uniformMatrix3fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix3fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 4 rows and 4 columns matrix for the current program object.
uniformMatrix4fv :: UniformLocation -> SettableStateVar [M44 GLfloat]
uniformMatrix4fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix4fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 3 rows and 2 columns matrix for the current program object.
uniformMatrix3x2fv :: UniformLocation -> SettableStateVar [V3 (V2 GLfloat)]
uniformMatrix3x2fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix2x3fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 2 rows and 3 columns matrix for the current program object.
uniformMatrix2x3fv :: UniformLocation -> SettableStateVar [V2 (V3 GLfloat)]
uniformMatrix2x3fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix3x2fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 4 rows and 2 columns matrix for the current program object.
uniformMatrix4x2fv :: UniformLocation -> SettableStateVar [V4 (V2 GLfloat)]
uniformMatrix4x2fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix2x4fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 2 rows and 4 columns matrix for the current program object.
uniformMatrix2x4fv :: UniformLocation -> SettableStateVar [V2 (V4 GLfloat)]
uniformMatrix2x4fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix4x2fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 4 rows and 3 columns matrix for the current program object.
uniformMatrix4x3fv :: UniformLocation -> SettableStateVar [V4 (V3 GLfloat)]
uniformMatrix4x3fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix3x4fv loc (len mats) 1) . castPtr))


-- | Specifies the value of a uniform variable which is an array of 3 rows and 4 columns matrix for the current program object.
uniformMatrix3x4fv :: UniformLocation -> SettableStateVar [V3 (V4 GLfloat)]
uniformMatrix3x4fv (UniformLocation loc) = makeSettableStateVar (\mats -> withArray mats ((glUniformMatrix4x3fv loc (len mats) 1) . castPtr))
