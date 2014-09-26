#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColor;

out vec4 col;

void main()
{
   col = vColor;
   gl_Position = vPosition;
}
