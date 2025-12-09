#include <iostream>
#include <vector>
#include <memory>
#include <utility>

using namespace std;


namespace solve {

template<typename ROOMS, typename FACTORY>
void solve(const int n) {
    vector<ROOMS*> rooms;

    for (int i = 0; i < n; ++i) {
        string room_type;
        int bedrooms;
        int bathrooms;
        cin >> room_type >> bedrooms >> bathrooms;

        rooms.push_back( FACTORY::make(room_type, bedrooms, bathrooms) );
    }


    int total_profit{ 0 };
    for (auto room : rooms) {
        total_profit += room->price();
    }
    cout << total_profit << endl;


    for (auto room : rooms) {
        delete room;
    }
    rooms.clear();
}

}


namespace templated {

#if __cplusplus >= 202002L
#include <concepts>

template<class T>
concept HasPrice = requires(const T& t) {
    { t.price() } -> std::convertible_to<int>;
}
#endif


class Priceable {
public:
#if __cplusplus >= 202002L
    // ctor from any type that satisfies the concept
    template<HasPrice T>
#else
    template<typename T>
#endif
    Priceable(T obj)
        : impl_(make_shared<Model<T>>(move(obj))) {}

    template<typename T>
    Priceable(T& ref)
        : impl_(make_shared<ModelRef<T>>(&ref)) {}

    template<typename T>
    Priceable(T* ptr)
        : impl_(make_shared<ModelRef<T>>(ptr)) {}

    // the interface operation
    int price() const { return impl_->price(); }

private:
    // abstract base for the erased implementation
    struct Concept {
        virtual ~Concept() = default;
        virtual int price() const = 0;
    };

    // concrete model that forwards to the real object
    template<class T>
    struct Model : Concept {
        explicit Model(T o) : obj_(move(o)) {}
        int price() const override { return static_cast<int>(obj_.price()); }
        T obj_;
    };

    template<class T>
    struct ModelRef : Concept {
        explicit ModelRef(T* p) : ptr_(p) {}
        int price() const override {
            if (!ptr_)
                throw runtime_error("Attempted to call price() on a null pointer");
            return static_cast<int>(ptr_->price());
        }
        T* ptr_;
    };

    shared_ptr<const Concept> impl_;
};


struct StandardRoom {
public:
    StandardRoom(const int bedrooms, const int bathrooms)
    : bedrooms_(bedrooms), bathrooms_(bathrooms) {}

    int price() const {
        return perBedroom * bedrooms_ + perBathroom * bathrooms_;
    }
private:
    int bedrooms_;
    int bathrooms_;
    const int perBedroom { 50 };
    const int perBathroom { 100 };
};


class ApartmentRoom {
public:
    ApartmentRoom(const int bedrooms, const int bathrooms)
    : bedrooms_(bedrooms), bathrooms_(bathrooms) {}

    int price() const {
        StandardRoom standard(bedrooms_, bathrooms_);
        return standard.price() + Surcharge;
    }
private:
    int bedrooms_;
    int bathrooms_;
    const int Surcharge{ 100 };
};


class Factory {
public:
    static Priceable* make(const string& room_type, const int bedrooms, const int bathrooms);
};

Priceable* Factory::make(const string& room_type, const int bedrooms, const int bathrooms) {
    if (room_type == "standard") {
        return new Priceable( StandardRoom(bedrooms, bathrooms) );
    } else {
        return new Priceable( ApartmentRoom(bedrooms, bathrooms) );
    }
}

// no recursion because
// this overload is a *different* function
inline void solve(const int n) {
    return solve::solve<Priceable, Factory>(n);
}

}


namespace interfaced {

class HotelRoom {
public:
    virtual int price() const = 0;
    virtual ~HotelRoom() = default;
};


class StandardRoom : public HotelRoom {
public:
    StandardRoom(const int bedrooms, const int bathrooms)
    : bedrooms_(bedrooms), bathrooms_(bathrooms) {}

    virtual int price() const override {
        return perBedroom * bedrooms_ + perBathroom * bathrooms_;
    }
private:
    int bedrooms_;
    int bathrooms_;
    const int perBedroom { 50 };
    const int perBathroom { 100 };
};


class ApartmentRoom : public HotelRoom {
public:
    ApartmentRoom(const int bedrooms, const int bathrooms)
    : bedrooms_(bedrooms), bathrooms_(bathrooms) {}

    virtual int price() const override {
        StandardRoom standard(bedrooms_, bathrooms_);
        return standard.price() + Surcharge;
    }
private:
    int bedrooms_;
    int bathrooms_;
    const int Surcharge{ 100 };
};


class Factory {
public:
    static HotelRoom* make(const string& room_type, const int bedrooms, const int bathrooms);
};

HotelRoom* Factory::make(const string& room_type, const int bedrooms, const int bathrooms) {
    if (room_type == "standard") {
        return new StandardRoom(bedrooms, bathrooms);
    } else {
        return new ApartmentRoom(bedrooms, bathrooms);
    }
}

// no recursion because
// this overload is a *different* function
inline void solve(const int n) {
    return solve::solve<HotelRoom, Factory>(n);
}

}


namespace given {

class HotelRoom {
public:
    HotelRoom(const int bedrooms, const int bathrooms)
    : bedrooms_(bedrooms), bathrooms_(bathrooms) {}

    virtual ~HotelRoom() = default;

    virtual int price() const {
        return perBedroom * bedrooms_ + perBathroom * bathrooms_;
    }
private:
    int bedrooms_;
    int bathrooms_;
    const int perBedroom { 50 };
    const int perBathroom { 100 };
};


class HotelApartment : public HotelRoom {
public:
    HotelApartment(const int bedrooms, const int bathrooms)
    : HotelRoom(bedrooms, bathrooms) {}

    int price() const override {
        return HotelRoom::price() + Surcharge;
    }
private:
    const int Surcharge{ 100 };
};


class Factory {
public:
    static HotelRoom* make(const string& room_type, const int bedrooms, const int bathrooms);
};

HotelRoom* Factory::make(const string& room_type, const int bedrooms, const int bathrooms) {
    if (room_type == "standard") {
        return new HotelRoom(bedrooms, bathrooms);
    } else {
        return new HotelApartment(bedrooms, bathrooms);
    }
}

// no recursion because
// this overload is a *different* function.
inline void solve(const int n) {
    return solve::solve<HotelRoom, Factory>(n);
}

}


int main() {
    int n;
    cin >> n;

    templated::solve(n);

    return EXIT_SUCCESS;
}
