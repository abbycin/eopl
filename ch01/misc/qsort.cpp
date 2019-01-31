#include <iostream>
#include <list>

template<typename T> T car(const std::list<T>& l)
{
  if(l.empty())
  {
    throw std::runtime_error("empty list");
  }
  return *l.begin();
}

template<typename T> std::list<T> cdr(const std::list<T>& l)
{
  if(l.empty())
  {
    throw std::runtime_error("empty list");
  }
  auto beg = l.begin();
  return {++beg, l.end()};
}

template<typename T, typename F> std::list<T> append(std::initializer_list<std::list<T>> arr, F pred)
{
  std::list<T> res;
  for(auto l: arr)
  {
    res.merge(l, pred);
  }
  return res;
}

template<typename T, typename F> std::list<T> filter(F pred, const std::list<T>& l)
{
  std::list<T> res;
  for(auto& x: l)
  {
    if(pred(x))
    {
      res.push_back(x);
    }
  }
  return res;
}

template<typename T, typename F> std::list<T> sort(std::list<T> l, F pred)
{
  if(l.empty())
  {
    return {};
  }
  else
  {
    return append({
          sort(filter([l, pred](T x) { return pred(car(l), x); }, cdr(l)), pred),
          std::list<T>{car(l)},
          sort(filter([l, pred](T x) { return !pred(car(l), x); }, cdr(l)), pred)
        }, pred);
  }
}

int main()
{
  auto print = [](auto& l) {
    for(auto x: l)
    {
      std::cerr << x << ' ';
    }
    std::cerr << '\n';
  };
  std::list<int> l{8, 2, 5, 2, 3};
  auto r = sort(l, [](auto l, auto r) { return l > r; });
  print(r);
  r = sort(l, [](auto l, auto r) { return l < r; });
  print(r);
}