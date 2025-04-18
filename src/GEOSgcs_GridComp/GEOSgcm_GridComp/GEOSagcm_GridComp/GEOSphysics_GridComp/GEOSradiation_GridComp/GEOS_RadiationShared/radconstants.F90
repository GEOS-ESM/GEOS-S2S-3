! $Id: radconstants.F90,v 1.2 2012/09/06 17:37:18 mathomp4 Exp $

module rad_constants

implicit none
save

   ! Constants for VIS-UV Radiation
   ! ------------------------------

   real :: aib_uv = 1.64

   real, dimension(2) :: awb_uv = [-6.59e-3,1.65]

   real, dimension(2) :: arb_uv = [ 3.07e-3,0.00]


   real, dimension(3) :: aig_uv = &
      [.746,  .00282,  -.0000230]

   real, dimension(3) :: awg_uv = &
      [.82562,.00529,  -.0001487]

   real, dimension(3) :: arg_uv = &
      [.883,0.0,0.0]

   ! Constants for NIR Radiation
   ! ---------------------------

   real :: aib_nir = 1.64 

   real, dimension(3,2) :: awb_nir = reshape([ &
     -0.0101, -0.0166, -0.0339, &
        1.72,    1.85,    2.16], [3,2])

   real, dimension(3,2) :: arb_nir = reshape([ &
      0.00307, 0.00307, 0.00307, &
      0.0    , 0.0    , 0.0    ],[3,2])


   real, dimension(3,3) :: aia_nir = reshape([ &
      .00000141,   .00112,     .04828, &
      .00001144,   .001129,    .00547, &
     -.000000005, -.00000358, -.0000361],[3,3])

   real, dimension(3,3) :: awa_nir = reshape([ &
     .00000007,-.00019934, .01209318, &
     .00000845, .00088757, .01784739, &
    -.00000004,-.00000650,-.00036910],[3,3])

   real, dimension(3,3) :: ara_nir = reshape([ &
     .029,      .342,      .466, &
     .0000,     .000,      .000, &
     .0000,     .000,      .000],[3,3])


   real, dimension(3,3) :: aig_nir = reshape([ &
     .725,      .717,       .771, &
     .0037,     .00456,     .00490, &
    -.0000309, -.00003544, -.0000401],[3,3])

   real, dimension(3,3) :: awg_nir = reshape([ &
     .79375035, .74513197, .83530748, &
     .00832441, .01370071, .00257181, &
    -.00023263,-.00038203, .00005519],[3,3])

   real, dimension(3,3) :: arg_nir = reshape([ &
     .891,      .948,      .971, &
     .0000,     .000,      .000, &
     .0000,     .000,      .000],[3,3])

   ! Constants used in both VIS-UV and NIR Radiation
   ! -----------------------------------------------

   real, dimension(11,9,11) :: caib = reshape( [& 
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,&
      0.068,0.088,0.094,0.096,0.097,0.098,0.098,0.099,0.099,0.100,0.101,&
      0.052,0.079,0.088,0.092,0.094,0.096,0.097,0.098,0.099,0.100,0.102,&
      0.038,0.065,0.079,0.086,0.090,0.092,0.094,0.096,0.098,0.100,0.102,&
      0.030,0.049,0.066,0.074,0.081,0.086,0.089,0.093,0.096,0.100,0.104,&
      0.025,0.037,0.050,0.061,0.069,0.075,0.081,0.086,0.092,0.098,0.106,&
      0.023,0.030,0.038,0.047,0.054,0.062,0.069,0.076,0.084,0.092,0.102,&
      0.022,0.026,0.031,0.037,0.042,0.049,0.056,0.063,0.071,0.081,0.091,&
      0.021,0.023,0.026,0.030,0.034,0.038,0.043,0.049,0.056,0.063,0.073,&
      0.021,0.022,0.023,0.026,0.028,0.030,0.034,0.038,0.042,0.046,0.053,&
      0.140,0.179,0.189,0.193,0.194,0.196,0.198,0.198,0.199,0.200,0.202,&
      0.106,0.161,0.178,0.186,0.190,0.193,0.194,0.196,0.198,0.200,0.202,&
      0.078,0.134,0.161,0.174,0.181,0.186,0.190,0.193,0.196,0.200,0.205,&
      0.060,0.102,0.134,0.153,0.165,0.174,0.180,0.186,0.193,0.199,0.207,&
      0.051,0.076,0.104,0.126,0.142,0.154,0.165,0.175,0.185,0.196,0.208,&
      0.046,0.061,0.080,0.098,0.114,0.129,0.142,0.156,0.170,0.185,0.202,&
      0.043,0.052,0.064,0.077,0.090,0.102,0.116,0.130,0.146,0.162,0.179,&
      0.042,0.047,0.054,0.062,0.070,0.080,0.090,0.102,0.114,0.128,0.142,&
      0.041,0.044,0.048,0.053,0.058,0.064,0.070,0.078,0.086,0.094,0.104,&
      0.216,0.272,0.285,0.290,0.293,0.295,0.296,0.298,0.298,0.300,0.302,&
      0.166,0.247,0.271,0.281,0.286,0.290,0.293,0.295,0.298,0.300,0.303,&
      0.120,0.207,0.247,0.264,0.274,0.281,0.286,0.290,0.295,0.300,0.306,&
      0.092,0.158,0.209,0.235,0.252,0.264,0.274,0.282,0.290,0.298,0.309,&
      0.078,0.118,0.163,0.195,0.218,0.237,0.252,0.266,0.280,0.294,0.309,&
      0.070,0.094,0.125,0.153,0.178,0.201,0.221,0.240,0.259,0.278,0.298,&
      0.066,0.081,0.100,0.120,0.141,0.162,0.182,0.203,0.224,0.246,0.267,&
      0.063,0.072,0.084,0.098,0.112,0.127,0.143,0.161,0.178,0.196,0.214,&
      0.062,0.066,0.074,0.082,0.090,0.100,0.111,0.122,0.134,0.146,0.158,&
      0.298,0.367,0.383,0.389,0.392,0.394,0.396,0.398,0.398,0.400,0.402,&
      0.230,0.337,0.366,0.378,0.384,0.389,0.392,0.394,0.398,0.400,0.404,&
      0.166,0.286,0.337,0.358,0.370,0.378,0.384,0.390,0.394,0.400,0.406,&
      0.126,0.221,0.289,0.323,0.343,0.358,0.369,0.379,0.389,0.398,0.410,&
      0.106,0.165,0.227,0.271,0.302,0.325,0.344,0.361,0.376,0.392,0.409,&
      0.095,0.130,0.175,0.215,0.250,0.279,0.306,0.330,0.351,0.374,0.395,&
      0.089,0.111,0.141,0.170,0.200,0.229,0.256,0.282,0.308,0.333,0.357,&
      0.086,0.098,0.118,0.138,0.159,0.182,0.203,0.226,0.248,0.270,0.290,&
      0.083,0.090,0.102,0.114,0.128,0.142,0.157,0.172,0.186,0.202,0.217,&
      0.385,0.465,0.482,0.488,0.492,0.494,0.496,0.497,0.498,0.500,0.502,&
      0.302,0.431,0.465,0.477,0.483,0.488,0.491,0.494,0.497,0.500,0.504,&
      0.218,0.372,0.431,0.455,0.468,0.477,0.483,0.489,0.494,0.500,0.506,&
      0.164,0.290,0.375,0.416,0.439,0.455,0.467,0.478,0.488,0.498,0.510,&
      0.136,0.217,0.300,0.355,0.392,0.419,0.440,0.458,0.474,0.491,0.508,&
      0.122,0.171,0.233,0.286,0.330,0.366,0.397,0.423,0.447,0.470,0.493,&
      0.114,0.146,0.188,0.230,0.269,0.305,0.338,0.369,0.398,0.424,0.449,&
      0.108,0.129,0.158,0.187,0.217,0.245,0.273,0.299,0.325,0.349,0.372,&
      0.105,0.118,0.136,0.154,0.174,0.192,0.210,0.229,0.246,0.264,0.281,&
      0.481,0.566,0.582,0.589,0.592,0.594,0.596,0.598,0.598,0.600,0.602,&
      0.383,0.531,0.565,0.578,0.584,0.588,0.591,0.594,0.598,0.600,0.604,&
      0.276,0.466,0.531,0.556,0.569,0.577,0.583,0.589,0.594,0.600,0.606,&
      0.206,0.370,0.470,0.514,0.539,0.556,0.568,0.578,0.588,0.598,0.609,&
      0.170,0.278,0.383,0.449,0.490,0.518,0.541,0.558,0.575,0.590,0.606,&
      0.150,0.221,0.302,0.370,0.422,0.462,0.496,0.523,0.547,0.570,0.590,&
      0.141,0.189,0.249,0.303,0.351,0.394,0.432,0.465,0.494,0.521,0.545,&
      0.135,0.170,0.213,0.252,0.289,0.323,0.355,0.385,0.412,0.438,0.462,&
      0.134,0.156,0.182,0.207,0.231,0.254,0.276,0.297,0.318,0.337,0.356,&
      0.586,0.669,0.685,0.690,0.693,0.695,0.696,0.698,0.699,0.700,0.702,&
      0.478,0.637,0.669,0.680,0.686,0.690,0.693,0.695,0.698,0.700,0.703,&
      0.346,0.572,0.637,0.660,0.672,0.680,0.686,0.690,0.695,0.700,0.706,&
      0.255,0.465,0.577,0.622,0.645,0.661,0.672,0.681,0.690,0.698,0.707,&
      0.209,0.354,0.483,0.555,0.598,0.626,0.646,0.663,0.678,0.691,0.705,&
      0.187,0.286,0.391,0.471,0.529,0.571,0.604,0.630,0.652,0.671,0.690,&
      0.187,0.259,0.336,0.401,0.455,0.501,0.540,0.572,0.601,0.625,0.647,&
      0.214,0.250,0.298,0.343,0.384,0.422,0.455,0.486,0.514,0.540,0.563,&
      0.202,0.224,0.254,0.282,0.309,0.334,0.359,0.382,0.405,0.426,0.446,&
      0.705,0.776,0.788,0.792,0.794,0.796,0.797,0.798,0.799,0.800,0.802,&
      0.595,0.749,0.776,0.785,0.789,0.792,0.794,0.796,0.798,0.800,0.802,&
      0.438,0.692,0.750,0.769,0.778,0.784,0.789,0.793,0.796,0.800,0.804,&
      0.322,0.583,0.697,0.737,0.757,0.769,0.778,0.786,0.792,0.798,0.805,&
      0.266,0.459,0.606,0.678,0.717,0.741,0.758,0.771,0.782,0.793,0.803,&
      0.278,0.398,0.518,0.600,0.656,0.694,0.722,0.744,0.762,0.777,0.790,&
      0.354,0.407,0.476,0.537,0.589,0.631,0.666,0.694,0.718,0.738,0.755,&
      0.349,0.387,0.433,0.476,0.515,0.550,0.583,0.611,0.638,0.661,0.681,&
      0.302,0.328,0.360,0.391,0.420,0.448,0.474,0.498,0.521,0.542,0.562,&
      0.840,0.886,0.894,0.896,0.897,0.898,0.898,0.899,0.899,0.900,0.901,&
      0.752,0.870,0.886,0.891,0.894,0.895,0.897,0.898,0.899,0.900,0.902,&
      0.582,0.831,0.870,0.882,0.887,0.891,0.894,0.896,0.898,0.900,0.902,&
      0.442,0.745,0.835,0.862,0.874,0.882,0.887,0.892,0.895,0.899,0.902,&
      0.462,0.638,0.770,0.823,0.850,0.865,0.875,0.883,0.890,0.896,0.902,&
      0.577,0.631,0.710,0.770,0.810,0.836,0.854,0.867,0.878,0.886,0.894,&
      0.603,0.643,0.689,0.729,0.764,0.793,0.816,0.834,0.850,0.862,0.872,&
      0.565,0.598,0.638,0.673,0.703,0.730,0.754,0.774,0.793,0.809,0.822,&
      0.479,0.508,0.542,0.574,0.602,0.627,0.650,0.672,0.691,0.710,0.726,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,&
      1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000], &
      [11,9,11])

   real, dimension(9,11) :: caif = reshape( [&
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.099,0.098, &
      0.096,0.092,0.085,0.076,0.063,0.052,0.043,0.198,0.196,0.192,0.185, &
      0.173,0.154,0.131,0.107,0.088,0.297,0.294,0.290,0.280,0.263,0.237, &
      0.203,0.166,0.136,0.397,0.394,0.388,0.376,0.357,0.324,0.281,0.232, &
      0.189,0.496,0.494,0.487,0.476,0.454,0.418,0.366,0.305,0.248,0.597, &
      0.594,0.587,0.576,0.555,0.517,0.461,0.389,0.317,0.697,0.694,0.689, &
      0.678,0.659,0.624,0.567,0.488,0.400,0.798,0.796,0.792,0.783,0.768, &
      0.738,0.688,0.610,0.510,0.899,0.898,0.895,0.890,0.881,0.864,0.830, &
      0.770,0.675,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000],&
      [9,11])

   ! Constants for IR Radiation
   ! --------------------------

   !*********************************************************************
   !   THE EQUATION NUMBERS noted in below follow the latest            !
   !    version (May 2003) of the NASA Tech. Memo. (2001), which can    !
   !    be accessed at ftp://climate.gsfc.nasa.gov/pub/chou/clirad_lw/  !
   !*********************************************************************

   !-----Coefficients for computing the extinction coefficient
   !     for cloud ice particles (Table 11a, Eq. 6.4a).
   !
   real, dimension(3,10) :: aib_ir = reshape([ &
      -0.44171,    0.61222,   0.06465,&
      -0.13727,    0.54102,   0.28962,&
      -0.01878,    1.19270,   0.79080,&
      -0.01896,    0.78955,   0.69493,&
      -0.04788,    0.69729,   0.54492,&
      -0.02265,    1.13370,   0.76161,&
      -0.01038,    1.46940,   0.89045,&
      -0.00450,    1.66240,   0.95989,&
      -0.00044,    2.01500,   1.03750,&
      -0.02956,    1.06430,   0.71283],[3,10])

   !-----coefficients for computing the extinction coefficient
   !     for cloud liquid drops. (Table 11b, Eq. 6.4b)
   !
   real, dimension(4,10) :: awb_ir = reshape([  &
       0.08641,    0.01769,    -1.5572e-3,   3.4896e-5,&
       0.22027,    0.00997,    -1.8719e-3,   5.3112e-5,&
       0.38074,   -0.03027,     1.0154e-3,  -1.1849e-5,&
       0.15587,    0.00371,    -7.7705e-4,   2.0547e-5,&
       0.05518,    0.04544,    -4.2067e-3,   1.0184e-4,&
       0.12724,    0.04751,    -5.2037e-3,   1.3711e-4,&
       0.30390,    0.01656,    -3.5271e-3,   1.0828e-4,&
       0.63617,   -0.06287,     2.2350e-3,  -2.3177e-5,&
       1.15470,   -0.19282,     1.2084e-2,  -2.5612e-4,&
       0.34021,   -0.02805,     1.0654e-3,  -1.5443e-5],[4,10])

   !-----coefficients for computing the single-scattering albedo
   !     for cloud ice particles. (Table 12a, Eq. 6.5)
   !
   real, dimension(4,10) :: aiw_ir = reshape([  &
       0.17201,    1.8814e-2,  -3.5117e-4,   2.1127e-6,&
       0.81470,   -4.1989e-3,   2.3152e-7,   2.0992e-7,&
       0.54859,   -7.4266e-4,   1.2865e-5,  -5.7092e-8,&
       0.39218,    6.4180e-3,  -1.1567e-4,   6.9710e-7,&
       0.71773,   -5.1754e-3,   4.6658e-5,  -1.2085e-7,&
       0.77345,   -8.4966e-3,   1.1451e-4,  -5.5170e-7,&
       0.74975,   -8.7083e-3,   1.3367e-4,  -7.1603e-7,&
       0.69011,   -6.9766e-3,   1.1674e-4,  -6.6472e-7,&
       0.83963,   -1.0347e-2,   1.4651e-4,  -7.5965e-7,&
       0.64860,   -4.4142e-3,   6.5458e-5,  -3.2655e-7],[4,10])

   !-----coefficients for computing the single-scattering albedo
   !     for cloud liquid drops. (Table 12b, Eq. 6.5)
   !
   real, dimension(4,10) :: aww_ir = reshape([  &
      -7.8566e-2,  8.0875e-2,  -4.3403e-3,   8.1341e-5,&
      -1.3384e-2,  9.3134e-2,  -6.0491e-3,   1.3059e-4,&
       3.7096e-2,  7.3211e-2,  -4.4211e-3,   9.2448e-5,&
      -3.7600e-3,  9.3344e-2,  -5.6561e-3,   1.1387e-4,&
       0.40212,    7.8083e-2,  -5.9583e-3,   1.2883e-4,&
       0.57928,    5.9094e-2,  -5.4425e-3,   1.2725e-4,&
       0.68974,    4.2334e-2,  -4.9469e-3,   1.2863e-4,&
       0.80122,    9.4578e-3,  -2.8508e-3,   9.0078e-5,&
       1.02340,   -2.6204e-2,   4.2552e-4,   3.2160e-6,&
       0.05092,    7.5409e-2,  -4.7305e-3,   1.0121e-4],[4,10])

   !-----coefficients for computing the asymmetry factor for cloud ice 
   !     particles. (Table 13a, Eq. 6.6)
   !
   real, dimension(4,10) :: aig_ir = reshape([  &
       0.57867,    1.5592e-2,  -2.6372e-4,   1.5125e-6,&
       0.72259,    4.7922e-3,  -4.7164e-5,   2.0400e-7,&
       0.76109,    6.9922e-3,  -1.0935e-4,   5.9885e-7,&
       0.86934,    4.2268e-3,  -7.4085e-5,   4.3547e-7,&
       0.89103,    2.8482e-3,  -3.9174e-5,   2.0098e-7,&
       0.86325,    3.2935e-3,  -3.9872e-5,   1.8015e-7,&
       0.85064,    3.8505e-3,  -4.9259e-5,   2.3096e-7,&
       0.86945,    3.7869e-3,  -5.6525e-5,   3.0016e-7,&
       0.80122,    4.9086e-3,  -5.8831e-5,   2.6367e-7,&
       0.73290,    7.3898e-3,  -1.0515e-4,   5.4034e-7],[4,10])

   !-----coefficients for computing the asymmetry factor for cloud liquid 
   !     drops. (Table 13b, Eq. 6.6)
   !
   real, dimension(4,10) :: awg_ir = reshape([  &
      -0.51930,    0.20290,    -1.1747e-2,   2.3868e-4,&
      -0.22151,    0.19708,    -1.2462e-2,   2.6646e-4,&
       0.14157,    0.14705,    -9.5802e-3,   2.0819e-4,&
       0.41590,    0.10482,    -6.9118e-3,   1.5115e-4,&
       0.55338,    7.7016e-2,  -5.2218e-3,   1.1587e-4,&
       0.61384,    6.4402e-2,  -4.6241e-3,   1.0746e-4,&
       0.67891,    4.8698e-2,  -3.7021e-3,   9.1966e-5,&
       0.78169,    2.0803e-2,  -1.4749e-3,   3.9362e-5,&
       0.93218,   -3.3425e-2,   2.9632e-3,  -6.9362e-5,&
       0.01649,    0.16561,    -1.0723e-2,   2.3220e-4],[4,10])

end module rad_constants
