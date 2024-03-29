// may be outdated
fn log(mut x: f64): f64 {
	let ln2_hi = <f64!>(0x3FE62E42FEE00000); // 6.93147180369123816490e-01
    let ln2_lo = <f64!>(0x3DEA39EF35793C76); // 1.90821492927058770002e-10
    let Lg1    = <f64!>(0x3FE5555555555593); // 6.666666666666735130e-01
    let Lg2    = <f64!>(0x3FD999999997FA04); // 3.999999999940941908e-01
    let Lg3    = <f64!>(0x3FD2492494229359); // 2.857142874366239149e-01
    let Lg4    = <f64!>(0x3FCC71C51D8E78AF); // 2.222219843214978396e-01
    let Lg5    = <f64!>(0x3FC7466496CB03DE); // 1.818357216161805012e-01
    let Lg6    = <f64!>(0x3FC39A09D078C69F); // 1.531383769920937332e-01
    let Lg7    = <f64!>(0x3FC2F112DF3E5244); // 1.479819860511658591e-01
    let Ox1p54 = <f64!>(0x4350000000000000); // 0x1p54

	let mut u = <u64!>x;
	let mut hx = <u32>(u >> 32);
	let mut k = 0;
	if hx < 0x00100000 | hx >> 31 {
		if (u << 1 == 0) return -1 / (x * x); // log(+-0) = -inf 
		if (hx >> 31) return (x - x) / 0.0; // log(-#) = NaN

		// subnormal number, scale x up
		k -= 54;
		x *= Ox1p54;
		u = <u64!>x;
		hx = <u32>(u >> 32);
	} else if hx >= 0x7FF00000 {
		return x;
	} else if hx == 0x3FF00000 & u << 32 == 0
		return 0;

	// reduce x into [sqrt(2)/2, sqrt(2)]
	hx += 0x3FF00000 - 0x3FE6A09E;
	k += (<i32>hx >> 20) - 0x3FF;
	hx = (hx & 0x000FFFFF) + 0x3FE6A09E;
	u = <u64>hx << 32 | (u & 0xFFFFFFFF);
	x = <f64!>u;

	let f = x - 1.0;
	let hfsq = 0.5 * f * f;
	let s = f / (2.0 + f);
	let z = s * s;
	let w = z * z;
	let t1 = w * (Lg2 + w * (Lg4 + w * Lg6));
	let t2 = z * (Lg1 + w * (Lg3 + w * (Lg5 + w * Lg7)));
	let r = t2 + t1;
	let dk = <f64>k;
	return s * (hfsq + r) + dk * ln2_lo - hfsq + f + dk * ln2_hi;
}

fn log10(mut x: f64): f64 {
	let n10hi     = <f64!>0x3FDBCB7B15200000; // 4.34294481878168880939e-01
	let ivln10lo  = <f64!>0x3DBB9438CA9AADD5; // 2.50829467116452752298e-11
	let log10_2hi = <f64!>0x3FD34413509F6000; // 3.01029995663611771306e-01
	let log10_2lo = <f64!>0x3D59FEF311F12B36; // 3.69423907715893078616e-13
	let Lg1       = <f64!>0x3FE5555555555593; // 6.666666666666735130e-01
	let Lg2       = <f64!>0x3FD999999997FA04; // 3.999999999940941908e-01
	let Lg3       = <f64!>0x3FD2492494229359; // 2.857142874366239149e-01
	let Lg4       = <f64!>0x3FCC71C51D8E78AF; // 2.222219843214978396e-01
	let Lg5       = <f64!>0x3FC7466496CB03DE; // 1.818357216161805012e-01
	let Lg6       = <f64!>0x3FC39A09D078C69F; // 1.531383769920937332e-01
	let Lg7       = <f64!>0x3FC2F112DF3E5244; // 1.479819860511658591e-01
	let Ox1p54    = <f64!>0x4350000000000000; // 0x1p54

	let mut u = <u64!>x;
	let mut hx = <u32>(u >> 32);
	let mut k = 0;
	if hx < 0x00100000 | hx >> 31 {
		if (u << 1 == 0) return -1 / (x * x); // log(+-0) = -inf 
		if (hx >> 31) return (x - x) / 0.0; // log(-#) = NaN

		// subnormal number, scale x up
		k -= 54;
		x *= Ox1p54;
		u = <u64!>x;
		hx = <u32>(u >> 32);
	} else if hx >= 0x7FF00000 {
		return x;
	} else if hx == 0x3FF00000 & u << 32 == 0
		return 0;
	
	// reduce x into [sqrt(2)/2, sqrt(2)]
	hx += 0x3FF00000 - 0x3FE6A09E;
	k += <i32>(hx >> 20) - 0x3FF;
	hx = (hx & 0x000FFFFF) + 0x3FE6A09E;
	u = <u64>hx << 32 | (u & 0xFFFFFFFF);
	x = <f64!>u;

	let mut f = x - 1.0;
	let mut hfsq = 0.5 * f * f;
	let mut s = f / (2.0 + f);
	let mut z = s * s;
	let mut w = z * z;
	let mut t1 = w * (Lg2 + w * (Lg4 + w * Lg6));
	let mut t2 = z * (Lg1 + w * (Lg3 + w * (Lg5 + w * Lg7)));
	let mut r = t2 + t1;

	// hi+lo = f - hfsq + s*(hfsq+R) ~ log(1+f)
	let mut hi = f - hfsq;
	u = <u64!>hi;
	u &= 0xFFFFFFFF00000000;
	hi = <f64!>u;
	let mut lo = f - hi - hfsq + s * (hfsq + r);

	// val_hi+val_lo ~ log10(1+f) + k*log10(2)
	let mut val_hi = hi * ivln10hi;
	let mut dk = <f64>k;
	let mut y = dk * log10_2hi;
	let mut val_lo = dk * log10_2lo + (lo + hi) * ivln10lo + lo * ivln10hi;

	/*
	 * Extra precision in for adding y is not strictly needed
	 * since there is no very large cancellation near x = sqrt(2) or
	 * x = 1/sqrt(2), but we do it anyway since it costs little on CPUs
	 * with some parallelism and it reduces the error for many args.
	 */
	w = y + val_hi;
	val_lo += (y - w) + val_hi;
	
	val_lo + w
}

fn log1p(mut x: f64): f64 {
    let ln2_hi = <f64!>0x3FE62E42FEE00000; // 6.93147180369123816490e-01
    let ln2_lo = <f64!>0x3DEA39EF35793C76; // 1.90821492927058770002e-10
    let Lg1    = <f64!>0x3FE5555555555593; // 6.666666666666735130e-01
    let Lg2    = <f64!>0x3FD999999997FA04; // 3.999999999940941908e-01
    let Lg3    = <f64!>0x3FD2492494229359; // 2.857142874366239149e-01
    let Lg4    = <f64!>0x3FCC71C51D8E78AF; // 2.222219843214978396e-01
    let Lg5    = <f64!>0x3FC7466496CB03DE; // 1.818357216161805012e-01
    let Lg6    = <f64!>0x3FC39A09D078C69F; // 1.531383769920937332e-01
    let Lg7    = <f64!>0x3FC2F112DF3E5244; // 1.479819860511658591e-01

	let mut u = <u64!>x;
	let mut hx = <u32>(u >> 32);
	let mut k = 1;
	let mut c = 0.0;
	let mut f = 0.0;
	if hx < 0x3FDA827A | hx >> 31 { // 1+x < sqrt(2)+
		if hx >= 0xBFF00000 { // x <= -1.0
			if (x == -1) return x / 0.0; // log1p(-1) = -inf
			return (x - x) / 0.0; // log1p(x<-1) = NaN
		};
		if hx << 1 < 0x3CA00000 << 1 { // |x| < 2**-53			
			return x;
		};
		if hx <= 0xBFD2BEC4 {
			k = 0;
			c = 0;
			f = x;
		};
	} else if hx >= 0x7FF00000
		return x;

	if (k) {
      u = <u64!>(1 + x);
      let mut hu = <u32>(u >> 32);
      hu += 0x3FF00000 - 0x3FE6A09E;
      k = <i32>(hu >> 20) - 0x3FF;
	  // correction term ~ log(1+x)-log(u), avoid underflow in c/u 
      if (k < 54) {
        let uf = <f64!>u;
        c = if k >= 2 {
			1 - (uf - x)
		} else {
			x - (uf - 1)
		};
        c /= uf;
      } else c = 0;
	  // reduce u into [sqrt(2)/2, sqrt(2)]
      hu = (hu & 0x000FFFFF) + 0x3FE6A09E;
      u = <u64>hu << 32 | (u & 0xFFFFFFFF);
      f = <f64!>u - 1;
    };

	let hfsq = 0.5 * f * f;
    let s = f / (2.0 + f);
    let z = s * s;
    let w = z * z;
    let t1 = w * (Lg2 + w * (Lg4 + w * Lg6));
    let t2 = z * (Lg1 + w * (Lg3 + w * (Lg5 + w * Lg7)));
    let r = t2 + t1;
    let dk = <f64>k;
    return s * (hfsq + r) + (dk * ln2_lo + c) - hfsq + f + dk * ln2_hi;
}