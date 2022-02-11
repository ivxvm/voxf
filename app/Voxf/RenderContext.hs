module Voxf.RenderContext where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Graphics.GL

data RenderContext = RenderContext
    { voxelVAO :: GLuint
    , voxelVBO :: GLuint
    }

init :: IO RenderContext
init = do
    vao <- alloca $ \ptr ->
        glGenVertexArrays 1 ptr >> peek ptr
    glBindVertexArray vao
    vbo <- alloca $ \ptr ->
        glGenBuffers 1 ptr >> peek ptr
    return $
        RenderContext
            { voxelVAO = vao
            , voxelVBO = vbo
            }
