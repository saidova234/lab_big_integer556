#include "big_integer.h"
static int cmp_abs(const std::vector<int>& a, const std::vector<int>& b) {
 if (a.size() != b.size()) {
 return a.size() < b.size() ? -1 : 1;
 }
 for (int i = static_cast<int>(a.size()) - 1; i >= 0; --i) {
 if (a[static_cast<size_t>(i)] != b[static_cast<size_t>(i)]) {
 return a[static_cast<size_t>(i)] < b[static_cast<size_t>(i)] ? -1 : 1;
 }
 }
 return 0;
}
static void add_abs(std::vector<int>& a, const std::vector<int>& b) {
 int carry = 0;
 size_t max_size = a.size() > b.size() ? a.size() : b.size();
 a.resize(max_size + 1, 0);
 for (size_t i = 0; i < max_size; ++i) {
 int sum = a[i] + (i < b.size() ? b[i] : 0) + carry;
 a[i] = sum % 10;
 carry = sum / 10;
 }
 if (carry > 0) {
 a[max_size] = carry;
 } else {
 while (a.size() > 1 && a.back() == 0) {
 a.pop_back();
 }
 }
}
static void sub_abs(std::vector<int>& a, const std::vector<int>& b) {
 int borrow = 0;
 for (size_t i = 0; i < a.size(); ++i) {
 int diff = a[i] - (i < b.size() ? b[i] : 0) - borrow;
 if (diff < 0) {
 diff += 10;
 borrow = 1;
 } else {
 borrow = 0;
 }
 a[i] = diff;
 }
 while (a.size() > 1 && a.back() == 0) {
 a.pop_back();
 }
}
static void mul_abs(std::vector<int>& a, const std::vector<int>& b) {
 std::vector<int> result(a.size() + b.size(), 0);
 for (size_t i = 0; i < a.size(); ++i) {
 int carry = 0;
 for (size_t j = 0; j < b.size(); ++j) {
 int prod = result[i + j] + a[i] * b[j] + carry;
 result[i + j] = prod % 10;
 carry = prod / 10;
 }
 if (carry > 0) {
 result[i + b.size()] += carry;
 }
 }
 while (result.size() > 1 && result.back() == 0) {
 result.pop_back();
 }
 a = result;
}
static std::vector<int> div_mod_abs(std::vector<int>& dividend,
 const std::vector<int>& divisor) {
 std::vector<int> quotient;
 std::vector<int> remainder;
 for (int i = static_cast<int>(dividend.size()) - 1; i >= 0; --i) {
 remainder.insert(remainder.begin(), dividend[static_cast<size_t>(i)]);
 while (remainder.size() > 1 && remainder.back() == 0) {
 remainder.pop_back();
 }
 int digit = 0;
 for (int d = 9; d >= 1; --d) {
 std::vector<int> temp = divisor;
 int carry = 0;
 for (size_t j = 0; j < temp.size(); ++j) {
 int prod = temp[j] * d + carry;
 temp[j] = prod % 10;
 carry = prod / 10;
 }
 while (carry > 0) {
 temp.push_back(carry % 10);
 carry /= 10;
 }
 while (temp.size() > 1 && temp.back() == 0) {
 temp.pop_back();
 }
 if (cmp_abs(temp, remainder) <= 0) {
 digit = d;
 sub_abs(remainder, temp);
 break;
 }
 }
 quotient.push_back(digit);
 }
 for (size_t l = 0, r = quotient.size() - 1; l < r; ++l, --r) {
 int tmp = quotient[l]; quotient[l] = quotient[r]; quotient[r] = tmp;
 }
 while (quotient.size() > 1 && quotient.back() == 0) {
 quotient.pop_back();
 }
 if (quotient.empty()) {
 quotient = {0};
 }
 if (remainder.empty()) {
 remainder = {0};
 }
 dividend = quotient;
 return remainder;
}
BigInteger::BigInteger() : digits_{0}, negative_{false} {}
BigInteger::BigInteger(int value) : BigInteger(static_cast<long long>(value)) {}
BigInteger::BigInteger(long long value) {
 negative_ = (value < 0);
 unsigned long long abs_value;
 if (negative_) {
 abs_value = static_cast<unsigned long long>(-(value + 1)) + 1ULL;
 } else {
 abs_value = static_cast<unsigned long long>(value);
 }
 do {
 digits_.push_back(static_cast<int>(abs_value % 10));
 abs_value /= 10;
 } while (abs_value > 0);
}
BigInteger::BigInteger(const std::string& str) {
 if (str.empty()) {
 digits_ = {0};
 negative_ = false;
 return;
 }
 size_t start = 0;
 if (str[0] == '-') {
 negative_ = true;
 start = 1;
 } else {
 negative_ = false;
 }
 for (int i = static_cast<int>(str.length()) - 1;
 i >= static_cast<int>(start); --i) {
 digits_.push_back(str[static_cast<size_t>(i)] - '0');
 }
 while (digits_.size() > 1 && digits_.back() == 0) {
 digits_.pop_back();
 }
 if (digits_.size() == 1 && digits_[0] == 0) {
 negative_ = false;
 }
}
std::string BigInteger::to_string() const {
 std::string result;
 if (negative_) {
 result += '-';
 }
 for (int i = static_cast<int>(digits_.size()) - 1; i >= 0; --i) {
 result += static_cast<char>(digits_[static_cast<size_t>(i)] + '0');
 }
 return result;
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
bool BigInteger::operator<(const BigInteger& rhs) const {
 if (negative_ != rhs.negative_) {
 return negative_;
 }
 int cmp = cmp_abs(digits_, rhs.digits_);
 return negative_ ? (cmp > 0) : (cmp < 0);
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
bool BigInteger::operator==(const BigInteger& rhs) const {
 return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}
bool BigInteger::operator!=(const BigInteger& rhs) const {
 return !(*this == rhs);
}
BigInteger BigInteger::operator-() const {
 BigInteger result = *this;
 if (!result.is_zero()) {
 result.negative_ = !result.negative_;
 }
 return result;
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
BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
 if (negative_ == rhs.negative_) {
 add_abs(digits_, rhs.digits_);
 return *this;
 }
 int cmp = cmp_abs(digits_, rhs.digits_);
 if (cmp == 0) {
 digits_ = {0};
 negative_ = false;
 return *this;
 }
 if (cmp > 0) {
 sub_abs(digits_, rhs.digits_);
 } else {
 std::vector<int> temp = rhs.digits_;
 sub_abs(temp, digits_);
 digits_ = temp;
 negative_ = rhs.negative_;
 }
 return *this;
}
BigInteger BigInteger::operator+(const BigInteger& rhs) const {
 BigInteger result = *this;
 result += rhs;
 return result;
}
BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
 *this += (-rhs);
 return *this;
}
BigInteger BigInteger::operator-(const BigInteger& rhs) const {
 BigInteger result = *this;
 result -= rhs;
 return result;
}
BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
 bool result_negative = (negative_ != rhs.negative_);
 mul_abs(digits_, rhs.digits_);
 negative_ = is_zero() ? false : result_negative;
 return *this;
}
BigInteger BigInteger::operator*(const BigInteger& rhs) const {
 BigInteger result = *this;
 result *= rhs;
 return result;
}
BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
 if (rhs.is_zero()) {
 return *this;
 }
 bool result_negative = (negative_ != rhs.negative_);
 if (cmp_abs(digits_, rhs.digits_) < 0) {
 digits_ = {0};
 negative_ = false;
 return *this;
 }
 std::vector<int> divisor = rhs.digits_;
 div_mod_abs(digits_, divisor);
 negative_ = is_zero() ? false : result_negative;
 return *this;
}
BigInteger BigInteger::operator/(const BigInteger& rhs) const {
 BigInteger result = *this;
 result /= rhs;
 return result;
}
BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
 if (rhs.is_zero()) {
 return *this;
 }
 bool remainder_negative = negative_;
 if (cmp_abs(digits_, rhs.digits_) < 0) {
 return *this;
 }
 std::vector<int> divisor = rhs.digits_;
 digits_ = div_mod_abs(digits_, divisor);
 negative_ = is_zero() ? false : remainder_negative;
 return *this;
}
BigInteger BigInteger::operator%(const BigInteger& rhs) const {
 BigInteger result = *this;
 result %= rhs;
 return result;
}
std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
 os << value.to_string();
 return os;
}
std::istream& operator>>(std::istream& is, BigInteger& value) {
 std::string s;
 is >> s;
 value = BigInteger(s);
 return is;
}
