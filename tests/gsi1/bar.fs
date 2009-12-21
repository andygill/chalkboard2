// mix{RGB}.fs
//
// Laplacian edge detection

uniform sampler2D sampler0;	// RGB 

void main(void)
{
	gl_FragColor.r = texture2D(sampler0,gl_TexCoord[0].st).r;
	gl_FragColor.g = 1.0-texture2D(sampler0,gl_TexCoord[0].st).r;
	gl_FragColor.b = texture2D(sampler0,gl_TexCoord[0].st).r;
        gl_FragColor.a = 1.0;
}
