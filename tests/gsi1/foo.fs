
uniform sampler2D sampler0;	// RGB 
uniform sampler2D sampler1;	// RGB
uniform sampler2D sampler2;	// Bool

void main(void)
{
	gl_FragColor.rgb = mix(
		texture2D(sampler0,gl_TexCoord[0].st).rgb,
		texture2D(sampler1,gl_TexCoord[0].st).rgb,
		texture2D(sampler2,gl_TexCoord[0].st).r);
        gl_FragColor.a = 1.0;
}
