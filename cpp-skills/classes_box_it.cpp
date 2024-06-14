#include<bits/stdc++.h>

using namespace std;


class Box {
public:
    Box() = default;

    Box(const int length, const int breadth, const int height)
        : length_{length}, breadth_{breadth}, height_{height}
        {}

    Box(const Box& b) = default;
    Box& operator=(const Box& b) = default;

    bool operator==(const Box& b) const noexcept {
        return tie(length_, breadth_, height_) == tie(b.length_, b.breadth_, b.height_);
    }

    bool operator<(const Box& b) const noexcept {
        return tie(length_, breadth_, height_) < tie(b.length_, b.breadth_, b.height_);
    }

    int get_length() const noexcept { return length_; }
    int get_breadth() const noexcept { return breadth_; }
    int get_height() const noexcept { return height_; }

    long long calc_volume() const noexcept { return (long long) length_ * breadth_ * height_; }

private:
    int length_;
    int breadth_;
    int height_;
};

ostream& operator<<(ostream& os, const Box& b) noexcept {
    os << b.get_length() << ' ' << b.get_breadth() << ' ' << b.get_height();
    return os;
}


void test_ostream(const Box& box) {
    cout << box << endl;
}

void test_volume(const Box& box) {
    cout << box.calc_volume() << endl;
}

void test_copy_assign(Box& box) {
    int l, b, h;
    cin >> l >> b >> h;

    Box another_box(l, b, h);
    box = another_box;
    cout << box << endl; 
}

void test_lesser(const Box& box) {
    int l, b, h;
    cin >> l >> b >> h;

    Box another_box(l, b, h);
    if (another_box < box) {
        cout << "Lesser" << "\n";
    }
    else {
        cout << "Greater" << "\n";
    }   
}

void test_copy_construct(Box& box) {
    Box another_box(box);
    cout << another_box << endl;    
}


void run_tests() {
    int n{};
    cin >> n;

    enum class qtype {
        output = 1,
        assign,
        lesser,
        volume,
        copy
    };

    Box box{};

    for (int i{0}; i < n; i++) {
        int in_type{};
        cin >> in_type;
        const auto type{ static_cast<qtype>(in_type) };

        switch (type) {
        case qtype::output:
            test_ostream(box);
            break;
        case qtype::assign:
            test_copy_assign(box);
            break;
        case qtype::lesser:
            test_lesser(box);
            break;
        case qtype::volume:
            test_volume(box);
            break;
        case qtype::copy:
            test_copy_construct(box);
            break;
        }
    }
}


int main() {
    run_tests();
}
