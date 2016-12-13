struct s{
    s() {}
    int n;
    int h;
}

template<typename T>
void g() {
    T t;
    t++;
    t--;
    t = 34;
}

int f() {
    g<int>();
    int j = 5;
    for (int i = 0; i < 20; ++i) {
        j++;
        if (j == 10
            break;
    }
    j--;
}