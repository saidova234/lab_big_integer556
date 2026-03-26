#include "big_integer.h"
#include <stdexcept>

static const int BASE = 1000000000;
static const int BASE_DIGITS = 9;

static void removeLeadingZeros(std::vector<int>& d) {
    while (d.size() > 1 && d.back() == 0) {
        d.pop_back();
    }
}

static int compareAbs(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size()) {
        return a.size() < b.size() ? -1 : 1;
    }
    for (int i = (int)a.size() - 1; i >= 0; i--) {
        if (a[i] != b[i]) {
            return a[i] < b[i] ? -1 : 1;
        }
    }
    return 0;
}

static std::vector<int> addAbs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result;
    long long carry = 0;

    int n = (int)std::max(a.size(), b.size());
    for (int i = 0; i < n || carry != 0; i++) {
        long long cur = carry;
        if (i < (int)a.size()) cur += a[i];
        if (i < (int)b.size()) cur += b[i];
        result.push_back((int)(cur % BASE));
        carry = cur / BASE;
    }

    return result;
}

static std::vector<int> subAbs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result;
    long long borrow = 0;

    for (int i = 0; i < (int)a.size(); i++) {
        long long cur = (long long)a[i] - borrow;
        if (i < (int)b.size()) cur -= b[i];

        if (cur < 0) {
            cur += BASE;
            borrow = 1;
        } else {
            borrow = 0;
        }

        result.push_back((int)cur);
    }

    removeLeadingZeros(result);
    return result;
}

BigInteger::BigInteger() {
    digits_.push_back(0);
    negative_ = false;
}

BigInteger::BigInteger(long long value) {
    negative_ = false;

    if (value < 0) {
        negative_ = true;
        unsigned long long uv;
        if (value == LLONG_MIN) {
            uv = (unsigned long long)LLONG_MAX + 1ULL;
        } else {
            uv = (unsigned long long)(-value);
        }
        while (uv > 0) {
            digits_.push_back((int)(uv % BASE));
            uv /= BASE;
        }
    } else {
        unsigned long long uv = (unsigned long long)value;
        if (uv == 0) {
            digits_.push_back(0);
            return;
        }
        while (uv > 0) {
            digits_.push_back((int)(uv % BASE));
            uv /= BASE;
        }
    }

    if (digits_.empty()) {
        digits_.push_back(0);
    }
}

BigInteger::BigInteger(int value) : BigInteger((long long)value) {}

BigInteger::BigInteger(const std::string& str) {
    negative_ = false;

    if (str.empty()) {
        throw std::invalid_argument("пустая строка");
    }

    int start = 0;
    if (str[0] == '-') {
        negative_ = true;
        start = 1;
    } else if (str[0] == '+') {
        start = 1;
    }

    while (start < (int)str.size() - 1 && str[start] == '0') {
        start++;
    }

    for (int i = start; i < (int)str.size(); i++) {
        if (!isdigit(str[i])) {
            throw std::invalid_argument("недопустимый символ в строке");
        }
    }

    for (int i = (int)str.size(); i > start; i -= BASE_DIGITS) {
        int from = i - BASE_DIGITS;
        if (from < start) from = start;
        digits_.push_back(std::stoi(str.substr(from, i - from)));
    }

    removeLeadingZeros(digits_);

    if (digits_.size() == 1 && digits_[0] == 0) {
        negative_ = false;
    }
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}

std::string BigInteger::to_string() const {
    std::string result;

    if (negative_) {
        result += '-';
    }

    result += std::to_string(digits_.back());

    for (int i = (int)digits_.size() - 2; i >= 0; i--) {
        std::string chunk = std::to_string(digits_[i]);
        while ((int)chunk.size() < BASE_DIGITS) {
            chunk = "0" + chunk;
        }
        result += chunk;
    }

    return result;
}

bool BigInteger::operator==(const BigInteger& rhs) const {
    return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ != rhs.negative_) {
        return negative_;
    }

    int c = compareAbs(digits_, rhs.digits_);

    if (negative_) {
        return c > 0;
    } else {
        return c < 0;
    }
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return !(rhs < *this);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

BigInteger BigInteger::operator-() const {
    BigInteger result = *this;
    if (!result.is_zero()) {
        result.negative_ = !result.negative_;
    }
    return result;
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    BigInteger result;

    if (negative_ == rhs.negative_) {
        result.digits_ = addAbs(digits_, rhs.digits_);
        result.negative_ = negative_;
    } else {
        int c = compareAbs(digits_, rhs.digits_);

        if (c == 0) {
            return BigInteger(0);
        } else if (c > 0) {
            result.digits_ = subAbs(digits_, rhs.digits_);
            result.negative_ = negative_;
        } else {
            result.digits_ = subAbs(rhs.digits_, digits_);
            result.negative_ = rhs.negative_;
        }
    }

    if (result.is_zero()) {
        result.negative_ = false;
    }

    return result;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    return *this + (-rhs);
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    *this = *this + rhs;
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this = *this - rhs;
    return *this;
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    BigInteger result;
    result.digits_.assign(digits_.size() + rhs.digits_.size(), 0);

    for (int i = 0; i < (int)digits_.size(); i++) {
        long long carry = 0;
        for (int j = 0; j < (int)rhs.digits_.size() || carry != 0; j++) {
            long long cur = (long long)result.digits_[i + j] + carry;
            if (j < (int)rhs.digits_.size()) {
                cur += (long long)digits_[i] * rhs.digits_[j];
            }
            result.digits_[i + j] = (int)(cur % BASE);
            carry = cur / BASE;
        }
    }

    removeLeadingZeros(result.digits_);

    result.negative_ = negative_ ^ rhs.negative_;

    if (result.is_zero()) {
        result.negative_ = false;
    }

    return result;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    *this = *this * rhs;
    return *this;
}

static std::vector<int> divBySingle(const std::vector<int>& a, int d, int& remainder) {
    std::vector<int> q(a.size(), 0);
    long long r = 0;

    for (int i = (int)a.size() - 1; i >= 0; i--) {
        long long cur = r * BASE + a[i];
        q[i] = (int)(cur / d);
        r = cur % d;
    }

    remainder = (int)r;
    removeLeadingZeros(q);
    return q;
}

static std::pair<std::vector<int>, std::vector<int>>
divAbs(const std::vector<int>& a, const std::vector<int>& b) {
    if (b.size() == 1) {
        int rem = 0;
        auto q = divBySingle(a, b[0], rem);
        return {q, {rem}};
    }

    std::vector<int> quotient(a.size(), 0);
    std::vector<int> remainder;

    for (int i = (int)a.size() - 1; i >= 0; i--) {
        remainder.insert(remainder.begin(), a[i]);
        removeLeadingZeros(remainder);

        int lo = 0, hi = BASE - 1, x = 0;
        while (lo <= hi) {
            int mid = lo + (hi - lo) / 2;

            std::vector<int> t(b.size() + 1, 0);
            long long carry = 0;
            for (int j = 0; j < (int)b.size() || carry != 0; j++) {
                long long cur = carry;
                if (j < (int)b.size()) cur += (long long)b[j] * mid;
                t[j] = (int)(cur % BASE);
                carry = cur / BASE;
            }
            removeLeadingZeros(t);

            if (compareAbs(t, remainder) <= 0) {
                x = mid;
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }

        quotient[i] = x;

        if (x > 0) {
            std::vector<int> t(b.size() + 1, 0);
            long long carry = 0;
            for (int j = 0; j < (int)b.size() || carry != 0; j++) {
                long long cur = carry;
                if (j < (int)b.size()) cur += (long long)b[j] * x;
                t[j] = (int)(cur % BASE);
                carry = cur / BASE;
            }
            removeLeadingZeros(t);
            remainder = subAbs(remainder, t);
        }
    }

    removeLeadingZeros(quotient);
    removeLeadingZeros(remainder);
    return {quotient, remainder};
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    if (rhs.is_zero()) {
        throw std::domain_error("деление на ноль");
    }

    if (compareAbs(digits_, rhs.digits_) < 0) {
        return BigInteger(0);
    }

    auto [qDigits, rDigits] = divAbs(digits_, rhs.digits_);

    BigInteger q;
    q.digits_ = qDigits;
    q.negative_ = negative_ ^ rhs.negative_;
    if (q.is_zero()) q.negative_ = false;
    return q;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    if (rhs.is_zero()) {
        throw std::domain_error("деление на ноль");
    }

    if (compareAbs(digits_, rhs.digits_) < 0) {
        return *this;
    }

    auto [qDigits, rDigits] = divAbs(digits_, rhs.digits_);

    BigInteger r;
    r.digits_ = rDigits;
    r.negative_ = negative_;
    if (r.is_zero()) r.negative_ = false;
    return r;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    *this = *this / rhs;
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    *this = *this % rhs;
    return *this;
}

BigInteger& BigInteger::operator++() {
    *this += BigInteger(1);
    return *this;
}

BigInteger& BigInteger::operator--() {
    *this -= BigInteger(1);
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    return os << value.to_string();
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string s;
    is >> s;
    value = BigInteger(s);
    return is;
}
