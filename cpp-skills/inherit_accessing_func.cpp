#include <iostream>
#include <array>
#include <algorithm>

using namespace std;


class Base {
public:
  virtual int func(int a) = 0;
  virtual int func_calls() const = 0;
  virtual int func_impl(int a) = 0;   // this method should not be in the interface
  
  virtual ~Base() = default;
};


/**
 * learn to use policy classes
 * and the troubles that come with it
 */
class CallsPolicy {
public:
  void called() { ++called_; }
  int calls() const { return called_; }
private:
  int called_{0};
};

template<class Policy>
class BaseWithPolicy : public Base, public Policy {
public:
  int func(int a) override {
    Policy::called();
    return func_impl(a);
  }
  int func_calls() const override {
    return Policy::calls();
  }
  // Derived must implement `int func_impl(int)`
};


class A : public BaseWithPolicy<CallsPolicy> {
public:
  int func_impl(int a) override {   // how can func() used here instead?
      return a*2;
  }
};

class B : public BaseWithPolicy<CallsPolicy> {
public:
  int func_impl(int a) override {
      return a*3;
  }
};

class C : public BaseWithPolicy<CallsPolicy> {
public:
  int func_impl(int a) override {
      return a*5;
  }
};


namespace {
  constexpr int SizeFactors{3};
  typedef array<pair<char,int>,SizeFactors> CallStats;
}


class D {
public:
  /**
   * find the prime factor serialization for _target_
   * caller guarantees only 5,3,2 are required
   * use the factor serialization to make correct number
   *   of calls to the _Factor Classes_
   *   the result should be _target_
   */
  CallStats update(const int target) {
    array<pair<int,int>,SizeFactors> factors = { {{5,0}, {3,0}, {2,0}} };
    array<pair<int,Base*>,SizeFactors> calls{ {{0, new C()}, {0, new B()}, {0, new A()}} };

    int remains{ target };
    for (int i{0}; i<factors.size(); ++i) {
      auto& [div, counter] = factors[i];
      while (remains % div == 0) {
        remains = remains / div;
        ++counter;
      }
      calls[i].first = counter;
    }

    // C++20
    // for (auto& [counter, klass]: ranges::reverse_view(calls)) {
    reverse(begin(calls), end(calls));
    for (auto [counter, klass]: calls) {
      // C++20
      // for (auto _ : views::iota(0, counter)) { 
      for (int i{0}; i<counter; ++i) {
        val_ = klass->func(val_);
      }
    }

    for (auto& [_, ptr] : calls) delete ptr;
    
    // call_stats and calls
    // are in the correct order!!
    CallStats call_stats { {{'A',0}, {'B',0}, {'C',0}} };
    for (int i{0}; i<calls.size(); ++i) {
      auto [counter, _] = calls[i];
      call_stats[i].second = counter;
    }
    return call_stats;
  }

  /**
   * Used by hackerrank tests. Do not delete.
   */
  void check(int);
 
private:
  int val_{1};
};


int main(void) {
  int number{0};
  cin >> number;
  
  D d{};
  CallStats call_stats = d.update(number);
  
  cout << "Value = " << number << endl;
  for (auto stats: call_stats) {
    cout << stats.first << "'s func called " << stats.second << " times" << endl;
  }
      
  return EXIT_SUCCESS;
}
