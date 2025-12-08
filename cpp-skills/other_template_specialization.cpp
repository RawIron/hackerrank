#include <iostream>
#include <string_view>


using namespace std;


#if __cplusplus >= 202002L && false
#include <format>

enum class Color { Red, Green, Blue };

template <>  
struct formatter<Color> : formatter<string_view> {  
  auto format(Color c, format_context& ctx) const {  
    string_view name;  
    switch (c) {
      case Color::Red:   name = "Red"; break;  
      case Color::Green: name = "Green"; break;  
      case Color::Blue:  name = "Blue"; break;  
      default:           name = "Unknown";  
    }  
    return formatter<string_view>::format(name, ctx);  
  }  
};

static_assert( format("{}", Color::Green) == "Green" );

#endif


#if __cplusplus >= 201703L && __cplusplus < 202002L && false
#endif


#if __cplusplus == 201402L && false

enum class Color { Red, Green, Blue };

// constexpr array of (enum, string) pairs  
constexpr array<pair<Color, string_view>, static_cast<size_t>(Color::Count)> colorNames = {  
  {Color::Red, "Red"},  
  {Color::Green, "Green"},  
  {Color::Blue, "Blue"}  
};  

// constexpr function to convert enum to string
constexpr string_view toString(Color c) {  
  for (const auto& pair : colorNames) {  
    if (pair.first == c) {  
      return pair.second;  
    }  
  }  
  return "Unknown";
}  

// Compile-time test
static_assert(toString(Color::Red) == "Red"); 
static_assert(toString(Color::Blue) == "Blue");

#endif


#if __cplusplus == 201103L || true

enum class Fruit { apple, orange, pear };
enum class Color { red, green, orange };


constexpr Fruit allFruits[] = { Fruit::apple, Fruit::orange, Fruit::pear };

inline const string toString(Fruit f) {
    switch (f) {
        case Fruit::apple:  return "apple";
        case Fruit::orange: return "orange";
        case Fruit::pear:   return "pear";
        default: return "unknown";
    }
}


constexpr Color allColors[] = { Color::red, Color::green, Color::orange };

inline const string toString(Color c) {
    switch (c) {
        case Color::red:    return "red";
        case Color::green:  return "green";
        case Color::orange: return "orange";
        default: return "unknown";
    }
}


template<typename T> struct Traits;

template<>
struct Traits<Color> {
public:
    static string name(const int id) {
        for (Color e : allColors) {
            if (static_cast<int>(e) == id) {
                return toString(e);
            }
        }
        return "unknown";
    }
};

template<>
struct Traits<Fruit> {
public:
    static string name(const int id) {
        for (Fruit e : allFruits) {
            if (static_cast<int>(e) == id) {
                return toString(e);
            }
        }
        return "unknown";
    }
};

#endif


int main() {
    int testsTotal{};
    cin >> testsTotal;

    for (int i{0}; i < testsTotal; ++i) {
        int color{};
        cin >> color;
        int fruit{};
        cin >> fruit;

        cout << Traits<Color>::name(color) << " ";
        cout << Traits<Fruit>::name(fruit) << endl;
    }
}
