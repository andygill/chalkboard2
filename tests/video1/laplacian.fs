// laplacian.fs
//
// Laplacian edge detection

uniform sampler2D sampler0;
uniform sampler2D sampler1;
uniform sampler2D sampler2;
uniform vec2 tc_offset[9];

void main(void)
{
    vec4 sample[9];

    for (int i = 0; i < 9; i++)
    {
        sample[i] = texture2D(sampler0, 
                              gl_TexCoord[0].st + tc_offset[i]);
    }

//   -1 -1 -1
//   -1  8 -1
//   -1 -1 -1

    float Kb = 0.114;
    float Kr = 0.299;
    float R = texture2D(sampler0,gl_TexCoord[0].st).r;
    float G = texture2D(sampler0,gl_TexCoord[0].st).g;
    float B = texture2D(sampler0,gl_TexCoord[0].st).b;

    float Y  =   + 0.299    * R + 0.587    * G + 0.114    * B;
    float Pb =   - 0.168736 * R - 0.331264 * G + 0.5      * B;
    float Pr =   + 0.5      * R - 0.418688 * G - 0.081312 * B;

    float R2 = texture2D(sampler1,gl_TexCoord[0].st).r;
    float G2 = texture2D(sampler1,gl_TexCoord[0].st).g;
    float B2 = texture2D(sampler1,gl_TexCoord[0].st).b;

    float Y2  =   + 0.299    * R2 + 0.587    * G2 + 0.114    * B2;
    float Pb2 =   - 0.168736 * R2 - 0.331264 * G2 + 0.5      * B2;
    float Pr2 =   + 0.5      * R2 - 0.418688 * G2 - 0.081312 * B2;

    vec4 fragColor = sqrt(abs(texture2D(sampler0,gl_TexCoord[0].st) - texture2D(sampler1,gl_TexCoord[0].st)));
/*
                    (sample[0] + sample[1] + sample[2] + 
                     sample[3] + sample[5] + 
                     sample[6] + sample[7] + sample[8]);
*/
    float front = fragColor.r + fragColor.g + fragColor.b;
//    if (abs (Pb - Pb2) * abs(Pr - Pr2) < 0.1) { 

// Voting. -1 is background, +1 is forground.

    float rVote = R * Y2 - R2 * Y;
    float gVote = G * Y2 - G2 * Y;
    float bVote = B * Y2 - B2 * Y;
    float PbVote = Pb * Y2 - Pb2 * Y;
    float PrVote = Pr * Y2 - Pr2 * Y;

    float dist = length(vec3(abs(rVote),abs(gVote),abs(bVote)));


    int vote = 0;
/*
    if (Y > 0.2) { // if bright, perhaps background
        vote -= 2;
    }
    if (abs (R * Y2 - R2 * Y) > 0.005) { // if red has moved, perhaps foreground
	 vote += 1;
    }
    if (abs (R * Y2 - R2 * Y) > 0.01) { // if red has moved, perhaps foreground
	 vote += 1;
    }
    if (abs (G * Y2 - G2 * Y) > 0.005) { // if red has moved, perhaps foreground
	 vote += 1;
    }
    if (abs (G * Y2 - G2 * Y) > 0.01) { // if red has moved, perhaps foreground
	 vote += 1;
    }
    if (abs (B * Y2 - B2 * Y) > 0.005) { // if red has moved, perhaps foreground
	 vote += 1;
    }

    if (vote == -2) {
 	gl_FragColor.rgb = vec3(1.0,0.0,0.0);
    } else if (vote == -1) {
 	gl_FragColor.rgb = vec3(0.5,0.0,0.0);
    } else if (vote == 0) {
 	gl_FragColor.rgb = vec3(0.0,0.5,0.0);
    } else if (vote == 1) {
 	gl_FragColor.rgb = vec3(0.0,1.0,0.0);
    } else if (vote == 2) {
 	gl_FragColor.rgb = vec3(0.0,1.0,0.5);
    } else if (vote == 3) {
 	gl_FragColor.rgb = vec3(0.0,1.0,1.0);
    } else if (vote == 4) {
 	gl_FragColor.rgb = vec3(0.5,1.0,1.0);
    } 

*/
    float v = 0.0;
    // Criteria 1 : are foreground if 0.3 darker.
    if(abs(Y - Y2) >= 0.3) {
	v += 2.0;
    } else if (abs(Y - Y2) <= 0.1) {
	v -= 1.0;
    }
    if (abs(Y) <= 0.2) {
	v += 2.0;
    }
    if (abs (R - Y) >= 0.03) {
	v += 2.0;
    } else if (abs (R - Y) <= 0.01) {
	v -= 0.1;
    }
    if (abs (G - Y) >= 0.02) {
	v += 2.0;
    } else if (abs (G - Y) <= 0.01) {
	v -= 0.1;
    }
    if (abs (B - Y) >= 0.05) {
	v += 2.0;
    } else if (abs (B - Y) <= 0.01) {
	v -= 0.1;
    }
    if (distance(vec2(Pb,Pr),vec2(Pb2,Pr2)) >= 0.2) {
	v += 5.0 * distance(vec2(Pb,Pr),vec2(Pb2,Pr2));
    }

    float len = length(texture2D(sampler0,gl_TexCoord[0].st).rgb) -
	        length(texture2D(sampler2,gl_TexCoord[0].st).rgb);
     if (len < -0.1) { 
		v += 2.0;
     } else if (len > 0.03) {
//		v -= 0.5;		
     }



 
    vec4 here = 
	(sample[4] * 8.0) - 
	(sample[0] + sample[1] + sample[2] + 
                     sample[3] + sample[5] + 
                     sample[6] + sample[7] + sample[8]);


    if (length(here.rgb) > 0.2) {
      v += 2.0;
    } else if (length(here.rgb) > 0.1) {
      v += 1.0;
    }

   float p = (v + 5.0) / 10.0;
//   float p = distance(texture2D(sampler0,gl_TexCoord[0].st),texture2D(sampler1,gl_TexCoord[0].st)) / 3.0;
//   float p = distance(vec2(Pb,Pr),vec2(Pb2,Pr2)) * 10.0;
	
    if ((gl_TexCoord[0].s > 0.05) && (gl_TexCoord[0].s < 0.7)
	&& (gl_TexCoord[0].t > 0.90)) {
	p = (gl_TexCoord[0].s - 0.05) * (1.0 / (0.7 - 0.05));
    }

    float p2 = floor(p * 10.0) / 2.0;

    if (p2 > 3.0) {
      p2 = p2 - 3.0;
      gl_FragColor.rgb = vec3(p2,p2,0);
    } else if (p2 > 2.0) {
      p2 = p2 - 2.0;
      gl_FragColor.rgb = vec3(p2,0,0);
    } else if (p2 > 1.0) {
      p2 = p2 - 1.0;
      gl_FragColor.rgb = vec3(0,p2,0);
    } else if (p2 > 0.0) {
      gl_FragColor.rgb = vec3(0,0,p2);
    } else {
      gl_FragColor.rgb = vec3(1,1,1);
    }


  if(true) {
    if (v >= 0.0
	|| (gl_TexCoord[0].s < 0.05  || gl_TexCoord[0].s > 0.7
		|| gl_TexCoord[0].t < 0.4  || gl_TexCoord[0].t > 0.9)) { 
	gl_FragColor.rgb = texture2D(sampler0,gl_TexCoord[0].st).rgb;	
        gl_FragColor.a = 1.0;
    } else {
	gl_FragColor.gb = texture2D(sampler0,gl_TexCoord[0].st).gb;
	gl_FragColor.r   = 1.1 * texture2D(sampler0,gl_TexCoord[0].st).r;
        gl_FragColor.a = 1.0;
    }
  } else {
/*
	gl_FragColor.rgb = (vec3(1.0,1.0,1.0) 
		- texture2D(sampler0,gl_TexCoord[0].st).rgb 
		+ texture2D(sampler2,gl_TexCoord[0].st).rgb) / vec3(2.0,2.0,2.0);
*/
	float len = length(texture2D(sampler0,gl_TexCoord[0].st).rgb) -
	 	    length(texture2D(sampler2,gl_TexCoord[0].st).rgb);
		if (len < -0.03) { 
		gl_FragColor.rgb = texture2D(sampler0,gl_TexCoord[0].st).rgb;	
	} else {
		gl_FragColor.rgb = vec3(1,1,1);		
	}
        gl_FragColor.a = 1.0;
  }
/*
    if (abs (R * Y2 - R2 * Y) > 0.005) { 
	rVote = 1;
    } else if (abs (R * Y2 - R2 * Y) < 0.005) { 

    }

    else if (abs (G * Y2 - G2 * Y) > 0.005) { 
	gl_FragColor.rgb = vec3((abs (R * Y2 - R2 * Y)),0.1,1.0); // red
    } else if (abs (B * Y2 - B2 * Y) > 0.005) { 
	gl_FragColor.rgb = vec3((abs (R * Y2 - R2 * Y)),1.0,0.5); // red
    } else {
	gl_FragColor.rgb = texture2D(sampler0,gl_TexCoord[0].st).rgb;
    }
*/

//    } else if (abs (G - G2) < 0.1) { 
//	gl_FragColor.rgb = vec3(1.0,0.1,0.1); // red
//    } else if (abs (Pb - Pb2) + abs(Pr - Pr2) < 0.01) { 
//	gl_FragColor.rgb = vec3(0.1,0.9,0.5); // green

//    gl_FragColor.rgb = fragColor.rgb;
/*
    gl_FragColor.gb = vec2(0.0,0.0); // fragColor.gb;
*/
//    gl_FragColor.rgb = sample[8].rgb;
//    gl_FragColor.r = 0.5;
}


// LIAM KAYLEIGH
