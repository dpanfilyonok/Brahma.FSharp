__kernel void matvec(__global const float *A,
                     __global const float *x,
                     uint ncols,
                     __global float *y)
{
    size_t i = get_global_id(0);
    __global float const *a = &A[i*ncols];
    float sum = 0.f;
    for (size_t j = 0; j < ncols; j++) {
        sum += a[j] * x[j];
    }
    y[i] = sum;
}
