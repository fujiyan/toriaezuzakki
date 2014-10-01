#version 330 core

uniform sampler2D texture;
in vec4 col;
in vec2 texCoord;

out vec4 fColor;

void main()
{
    vec4 smpColor = texture2D(texture, texCoord);
    fColor = col * smpColor;
}
