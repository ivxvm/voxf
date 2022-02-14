module Voxf.RenderContext where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Graphics.GL

data RenderContext = RenderContext
    { vao :: GLuint
    , vbo :: GLuint
    }

init :: IO RenderContext
init = do
    vao <- alloca $ \ptr ->
        glGenVertexArrays 1 ptr >> peek ptr
    glBindVertexArray vao
    vbo <- alloca $ \ptr ->
        glGenBuffers 1 ptr >> peek ptr
    -- glBindBuffer GL_ARRAY_BUFFER vbo
    -- glBufferData GL_ARRAY_BUFFER
    return $
        RenderContext
            { vao = vao
            , vbo = vbo
            }
