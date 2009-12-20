// laplacian.fs
//
// Laplacian edge detection

uniform sampler2D sampler0;
uniform sampler2D sampler1;
uniform vec2 tc_offset[9];

void main(void)
{
	if (sampler0,gl_TexCoord[0].t > 0.5) { 
		gl_FragColor.rgb = texture2D(sampler0,gl_TexCoord[0].st).rgb;	
	} else {
		gl_FragColor.rgb = texture2D(sampler1,gl_TexCoord[0].st).rgb;
	}
        gl_FragColor.a = 1.0;
}
