unsigned int
generic_rotr32 (const unsigned int x, const unsigned bits)
{
  const unsigned n = bits % 32;
  return (x >> n) | (x << (32 - n));
}

struct aes_ctx
{
  unsigned int E[60];
};

void
aes_set_key (void *ctx_arg, unsigned int key_len, unsigned int *flags, unsigned int i, unsigned int t)
{
  struct aes_ctx *ctx = ctx_arg;
  switch (key_len)
    {
    case 16:
      {
	t = generic_rotr32 (t, 8);;
	ctx->E[8 * i + 15] = t;
      };
    }
}

