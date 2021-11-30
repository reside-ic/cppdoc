namespace ex {

using real_type = float;

// this is an example that occurs within dust
__host__ __device__
real_type add_gpu(real_type x, real_type y) {
  return x + y;
}

}
