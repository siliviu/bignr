# bignr

Very fast class for handling arbitrary sized integers in C++. There are 3 different files, each corresponding to a different container used to store the numbrers (T*, vector<T> and deque<T>)

Operations supported:
* addition
* subtraction
* multiplication (`+` operator is O(n^2); `multiply` method uses Karatsuba)
* division
* root
* others (comparison, increment, decrement, etc)

You can customise the size (only for the C style vector version), the underlying type used for storing the numbers and the base used. `bignr<SIZE, 9, unsigned long long, 1000000000u>` should be the fastest on current architectures .
