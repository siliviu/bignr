#include <iostream>
#include <iomanip>
#include <vector>
#include <cstring>

template<int LMAX, unsigned NGR = 1, class U = unsigned, unsigned GR = 10>
struct bignr {
public:
	static inline U c[35][35];
	U Number[LMAX];
	int Length;
	template <class T>
	bignr(T nr) {
		memset(Number, 0, sizeof(Number));
		Length = 0;
		do {
			Number[Length++] = nr % GR;
			nr /= GR;
		} while (nr);
	}
	bignr() : Length(1) {
		memset(Number, 0, sizeof(Number));
	}
	friend std::istream& operator >> (std::istream& is, bignr<LMAX, NGR, U, GR>& a) {
		memset(a.Number, 0, sizeof(a.Number));
		std::string nr;
		is >> nr;
		a.Length = nr.length() / NGR;
		int i = a.Length - 1, j = 0;
		for (int k = 0; k < nr.length() % NGR; ++k)
			a.Number[i + 1] = a.Number[i + 1] * 10 + (nr[j++] - '0');
		for (int k = a.Length; k >= 1; --k, --i)
			for (int l = 0; l < NGR; ++l)
				a.Number[i] = a.Number[i] * 10 + (nr[j++] - '0');
		if (nr.length() % NGR) a.Length++;
		return is;
	}
	friend std::ostream& operator << (std::ostream& os, const bignr<LMAX, NGR, U, GR>& a) {
		os << a.Number[a.Length - 1];
		for (int i = a.Length - 2; i >= 0; --i)
			os << std::setw(NGR) << std::setfill('0') << a.Number[i];
		return os;
	}
	bignr& operator <<= (int x) {
		if (Length == 1 && Number[0] == 0)
			return *this;
		memmove(Number + x, Number, sizeof(Number[0]) * Length);
		memset(Number, 0, sizeof(Number[0]) * x);
		Length += x;
		return *this;
	}
	bignr& operator >>= (int x) {
		if (x >= Length)
			*this = 0;
		else {
			memcpy(Number, Number + x, sizeof(Number[0]) * (Length - x));
			memset(Number + Length - x, 0, sizeof(Number[0]) * x);
			Length -= x;
		}
		return *this;
	}
	bignr& operator += (const bignr& nr2) {
		for (int i = 0; i < nr2.Length; ++i) {
			Number[i] += nr2.Number[i];
			if (Number[i] >= GR)
				Number[i] -= GR, ++Number[i + 1];
		}
		for (int i = nr2.Length; i < LMAX; ++i) {
			if (Number[i] >= GR)
				Number[i] -= GR, ++Number[i + 1];
			else {
				Length = Length > i + 1 - !Number[i] ? Length : i + 1 - !Number[i];
				break;
			}
		}
		return *this;
	}
	bignr& operator -= (const bignr& nr2) {
		for (int i = 0; i < nr2.Length; ++i)
			if (Number[i] >= nr2.Number[i])
				Number[i] -= nr2.Number[i];
			else {
				int j = i + 1;
				while (!Number[j])
					Number[j++] = GR - 1;
				--Number[j];
				Number[i] += (GR - nr2.Number[i]);
			}
		return FixRedundantZero();
	}
	template<typename T, typename = typename std::enable_if<!std::is_base_of<bignr, T>::value>::type>
	bignr& operator *= (const T& nr2) {
		if (*this == 0 || nr2 == 0)
			return (*this = 0);
		U carry = 0;
		for (int i = 0; i < Length; ++i) {
			U temp = carry + Number[i] * nr2;
			Number[i] = temp % GR;
			carry = temp / GR;
		}
		if (carry)
			Number[Length++] = carry;
		return *this;
	}
	bignr& operator *= (const bignr& nr2) {
		if (*this == 0 || nr2 == 0)
			return (*this = 0);
		if (Length == 1) {
			bignr temp = nr2;
			std::swap(*this, temp);
			return *this *= temp.Number[0];
		}
		if (nr2.Length == 1)
			return (*this *= nr2.Number[0]);
		bignr result = 0;
		result.Length = Length + nr2.Length - 1;
		for (int i = 0; i < Length; ++i)
			for (int j = 0; j < nr2.Length; ++j) {
				U temp = result.Number[i + j] + Number[i] * nr2.Number[j];
				result.Number[i + j] = temp % GR;
				result.Number[i + j + 1] += temp / GR;
			}
		if (result.Number[Length + nr2.Length - 1])
			++result.Length;
		return *this = result;
	}
	template<typename T, typename = typename std::enable_if<!std::is_base_of<bignr, T>::value>::type>
	bignr& operator /= (const T& nr2) {
		U r = 0;
		for (int i = Length - 1; i >= 0; --i) {
			r = GR * r + Number[i];
			Number[i] = r / nr2;
			r %= nr2;
		}
		return FixRedundantZero();
	}
	bignr& operator /= (const bignr& nr2) {
		if (nr2.Length == 1)
			return (*this /= nr2.Number[0]);
		if (*this < nr2)
			return *this = 0;
		int expl = Length - nr2.Length;
		for (int i1 = Length - 1, i2 = nr2.Length - 1; i1 >= 0 && i2 >= 0; --i1, --i2)
			if (Number[i1] != nr2.Number[i2]) {
				if (Number[i1] < nr2.Number[i2])
					--expl;
				break;
			}
		bignr bx = 0, x = 0;
		for (int i = expl; i >= 0; --i) {
			U left = 0, right = GR - 1;
			while (left < right) {
				U m = (left + right + 1) / 2;
				if (bx + ((nr2 * m) << i) <= *this)
					left = m;
				else
					right = m - 1;
			}
			bx += (nr2 * left) << i;
			x += (bignr)(left) << i;
		}
		return (*this = x);
	}
	bignr root(int k = 2) {
		if (!c[0][0]) {
			c[0][0] = 1;
			for (int i = 1; i < 35; ++i) {
				c[i][0] = 1;
				for (int j = 1; j <= i; ++j)
					c[i][j] = c[i - 1][j - 1] + c[i - 1][j];
			}
		}
		int expl = (Length - 1) / k;
		std::vector<bignr> b(k + 1), bans(k + 1), bnew(k + 1);
		b[0] = bnew[0] = 1;
		for (int i = expl; i >= 0; --i) {
			U left = 0, right = GR - 1;
			while (left < right) {
				U m = (left + right + 1) / 2;
				bnew[1] = b[1] + ((bignr)m << i);
				for (int j = 2; j <= k; ++j) {
					bignr cur = m;
					bnew[j] = b[j];
					for (int l = j - 1; l >= 0; --l) {
						bnew[j] += ((b[l] * c[j][j - l] * cur) << (i * (j - l)));
						cur *= m;
					}
				}
				if (bnew[k] <= *this)
					left = m, bans = bnew;
				else
					right = m - 1;
			}
			b = bans;
		}
		return b[1];
	}
	bignr multiply(bignr nr2) {
		bignr& nr1 = *this;
		int l1 = nr1.Length, l2 = nr2.Length;
		if (l1 == 1 || l2 == 1)
			return nr1 * nr2;
		int h = std::max(l1 / 2, l2 / 2);
		bignr a = nr1 >> h, b = nr1 - (a << h), c = nr2 >> h, d = nr2 - (c << h);
		bignr ac = a.multiply(c), bd = b.multiply(d), e = (a + b).multiply(c + d);
		return (ac << (2 * h)) + ((e - ac - bd) << h) + bd;
	}
	template<typename T, typename = typename std::enable_if<!std::is_base_of<bignr, T>::value>::type>
	bignr& operator %= (const T& nr2) {
		U r = 0;
		for (int i = Length - 1; i >= 0; --i)
			r = (GR * r + Number[i]) % nr2;
		return (*this = r);
	}
	bignr& operator %= (const bignr& nr2) {
		return *this -= (*this / nr2) * nr2;
	}
	template<class T>
	bignr& operator ^= (T nr2) {
		bignr ans = 1, nr1 = *this;
		while (nr2) {
			if (nr2 & 1)
				ans *= nr1;
			nr2 >>= 1;
			if (nr2)
				nr1 *= nr1;
		}
		return *this = ans;
	}
	bignr& operator ++() { return *this += 1; }
	bignr operator ++(int x) { bignr temp = *this; ++(*this); return temp; }
	bignr& operator --() { return *this -= 1; }
	bignr operator --(int x) { bignr temp = *this; --(*this); return temp; }
	bignr operator << (int x) const {
		bignr temp = *this;
		return temp <<= x;
	}
	bignr operator >> (int x) const {
		bignr temp = *this;
		return temp >>= x;
	}
	bignr operator + (const bignr& nr2) const {
		bignr temp = *this, temp2 = nr2;
		return temp.Length < temp2.Length ? temp2 += temp : temp += temp2;
	}
	bignr operator - (const bignr& nr2) const {
		bignr temp = *this;
		return temp -= nr2;
	}
	template<class T>
	bignr operator * (const T& nr2) const {
		bignr temp = *this;
		return temp *= nr2;
	}
	template<class T>
	bignr operator / (const T& nr2) const {
		bignr temp = *this;
		return temp /= nr2;
	}
	template<class T>
	bignr operator % (const T& nr2) const {
		bignr temp = *this;
		return temp %= nr2;
	}
	template<class T>
	bignr operator ^ (T nr2) const {
		bignr temp = *this;
		return temp ^= nr2;
	}
	bool operator !() const { return *this == 0; }
	bool operator < (const bignr& nr2) const { return Compare(nr2) < 0; }
	bool operator <= (const bignr& nr2) const { return Compare(nr2) <= 0; }
	bool operator == (const bignr& nr2) const { return !Compare(nr2); }
	bool operator > (const bignr& nr2) const { return Compare(nr2) > 0; }
	bool operator >= (const bignr& nr2) const { return Compare(nr2) >= 0; }
	bool operator != (const bignr& nr2) const { return Compare(nr2); }
private:
	int Compare(const bignr& nr2) const {
		if (Length != nr2.Length)
			return Length > nr2.Length ? 1 : -1;
		int i = Length - 1;
		while (i >= 0 && Number[i] == nr2.Number[i]) --i;
		if (i < 0)
			return 0;
		return Number[i] > nr2.Number[i] ? 1 : -1;
	}
	bignr& FixRedundantZero() {
		for (int i = Length - 1; i >= 0; --i)
			if (!Number[i] && Length > 1)
				--Length;
			else
				return *this;
	}
};

#include <iostream>

int main() {
	bignr<890, 9, unsigned long long, 1000000000u> a, b;
	std::cin >> a >> b;
	std::cout << a * b << std::endl;
	std::cout << a.multiply(b) << std::endl;
}