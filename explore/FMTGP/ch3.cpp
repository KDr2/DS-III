#include <cstdio>
#include <vector>

#define RandomAccessIterator typename
#define Integer typename

template <RandomAccessIterator I, Integer N>
void mark_sieve(I first, I last, N factor) {
    // assert(first != last);
    *first = false;
    while(last - first > factor) {
        first += factor;
        *first = false;
    }
}

template <RandomAccessIterator I, Integer N>
void sift0(I first, N n) {
    std::fill(first, first + n, true);

    N i(0);
    N index_square(3);
    while(index_square < n) {
        if(first[i]) {
            mark_sieve(
                first + index_square,
                first + n,
                2 * i + 3
            );
        }
        ++i;
        index_square = 2 * i * (i + 3) + 3;
    }
}

int main(int argc, char* argv[]) {
    int max = 100;
    int len = (max - 1) / 2;
    std::vector<bool> odds(len);
    sift0(odds.begin(), len);
    for(int i = 0; i < odds.size(); i++) {
        if(odds[i]) {
            printf("%d\n", 2 * i + 3);
        }
    }
}
