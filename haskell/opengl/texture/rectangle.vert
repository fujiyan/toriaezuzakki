#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColor;
layout(location = 2) in vec2 vTexCoord;

out vec4 col;
out vec2 texCoord;

void main()
{
    col = vColor;
    texCoord = vTexCoord;
    gl_Position = vPosition;
}
