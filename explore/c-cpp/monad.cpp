#include <functional>
#include <iostream>
#include <string>

template <template<typename> typename M, typename A>
M<A> ret(A a) { return M<A>(a); }

template <template<typename> typename M, typename A, typename B>
M<B> bind(M<A> ma, std::function<M<B>(A)> f) {
    if (ma.has_value()) {
        A a = ma.get();
        M<B> mb = f(a);
        return mb;
    } else {
        return M<B>::nothing();
    }
}


template <typename T>
class Maybe {
public:
    Maybe() : has_val(false) {}
    Maybe(T v) : has_val(true), val(v) {}

    bool has_value() { return has_val; }
    T get() { return val; }
    static Maybe nothing() {
        return Maybe<T>();
    }
private:
    bool has_val;
    T val;
};


int main(int argc, char** argv) {
    std::function<Maybe<std::string>(int)> f = [](int a) -> Maybe<std::string> {
        std::string s(a * 2, '=');
        return Maybe<std::string>(s);
    };

    // Maybe<int> a = Maybe<int>(3);
    Maybe<int> a = ret<Maybe, int>(3);
    auto b = bind(a, f);
    std::cout << b.get() << "\n";

    Maybe<int> n = Maybe<int>::nothing();
    auto m = bind(n, f);
    std::cout << m.has_value() << "\n";


}
