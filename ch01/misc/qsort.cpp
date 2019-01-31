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

template<typename T> std::list<T> append(std::initializer_list<std::list<T>> arr)
{
  std::list<T> res;
  for(auto l: arr)
  {
    res.merge(std::move(l));
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
        });
  }
}

int main()
{
  std::list<int> l{8, 2, 5, 2, 3};
  l = sort(l, [](auto l, auto r) { return l < r; });
  for(auto x: l)
  {
    std::cerr << x << ' ';
  }
  std::cerr << '\n';
}