#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColor;

uniform mat4x4 world;

out vec4 col;

void main()
{
    col = vColor;
    gl_Position = world * vPosition;
}
