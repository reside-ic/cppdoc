namespace ex {

using real_type_gpu = float;

// this is an example that occurs within dust
__host__ __device__
real_type_gpu add_gpu(real_type_gpu x, real_type_gpu y) {
  return x + y;
}

}
