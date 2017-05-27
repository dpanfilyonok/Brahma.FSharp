__kernel void assignArr(__global int* a, const int val) {
    const int id = get_global_id(0);
    a[id] = 42;
}
