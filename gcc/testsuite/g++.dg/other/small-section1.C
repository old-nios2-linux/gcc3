
extern "C"
{
extern int X12345[165000];
}

int X12345[165000];
int main() {return X12345[0];}
       

typedef long size_t;
typedef int _Atomic_word;
_Atomic_word __exchange_and_add (volatile _Atomic_word * __mem, int __val)
{
}

namespace
{
  template < typename _Facet > class __locale_cache;
  class locale
  {
  public:
    class facet;
    class id;
  };
  class locale::facet
  {
  };
  class locale::id
  {
    _Atomic_word _S_highwater;
  public:
    size_t _M_id ()
    {
      __exchange_and_add (&_S_highwater, 1);
    }
  };
  template < typename _CharT > class numpunctXXX
  {
  public:
    static locale::id id;
  };
  template < typename
    _Facet >
    const __locale_cache < _Facet > &__use_cache (const locale & __loc)
  {
    _Facet::id._M_id ();
  }
  template
    const
    __locale_cache < numpunctXXX < char > >&__use_cache < numpunctXXX <
    char > >(const locale & __loc);
}
