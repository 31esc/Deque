#pragma once

#include <algorithm>
#include <cstddef>
#include <deque>
#include <iostream>
#include <iterator>
#include <type_traits>

template <typename T>
class Deque {
 public:
  template <bool IsConst>
  class basic_iterator;

  using iterator = basic_iterator<false>;
  using const_iterator = basic_iterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  template <bool IsConst>
  class basic_iterator {
   public:
    using value_type = std::conditional_t<IsConst, const T, T>;
    using reference = std::conditional_t<IsConst, const T&, T&>;
    using pointer = std::conditional_t<IsConst, const T*, T*>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    basic_iterator() = delete;

    basic_iterator(T** ptr, size_t index) : ptr_(ptr), index_(index) {}

    reference operator*() const { return *((*ptr_) + index_); }

    pointer operator->() const { return &(**this); }

    difference_type operator-(const basic_iterator& it) const {
      return (ptr_ - it.ptr_) * BLOCK_SIZE + index_ - it.index_;
    }

    basic_iterator operator+(const difference_type& difference) const {
      basic_iterator copy(*this);
      copy += difference;
      return copy;
    }

    basic_iterator operator-(const difference_type& difference) const {
      basic_iterator copy(*this);
      copy -= difference;
      return copy;
    }

    basic_iterator& operator++() { return *this += 1; }

    basic_iterator& operator--() { return *this -= 1; }

    basic_iterator operator++(int) {
      basic_iterator copy = *this;
      ++(*this);
      return copy;
    }

    basic_iterator operator--(int) {
      basic_iterator copy = *this;
      --(*this);
      return copy;
    }

    basic_iterator& operator+=(const difference_type& difference) {
      if (difference < 0) {
        return *this -= (-difference);
      }
      index_ += difference;
      ptr_ += index_ / BLOCK_SIZE;
      index_ %= BLOCK_SIZE;
      return *this;
    }

    basic_iterator& operator-=(const difference_type& difference) {
      if (difference < 0) {
        return *this += (-difference);
      }
      index_ = BLOCK_SIZE + difference - index_ - 1;
      ptr_ -= (index_ / BLOCK_SIZE);
      index_ = (BLOCK_SIZE - 1 - (index_ % BLOCK_SIZE)) % BLOCK_SIZE;
      return *this;
    }

    bool operator<(const basic_iterator& other) const {
      return ptr_ < other.ptr_ || (ptr_ == other.ptr_ && index_ < other.index_);
    }

    bool operator>(const basic_iterator& other) const { return other < *this; }

    bool operator<=(const basic_iterator& other) const {
      return !(*this > other);
    }

    bool operator>=(const basic_iterator& other) const {
      return !(*this < other);
    }

    bool operator==(const basic_iterator& other) const {
      return index_ == other.index_ && ptr_ == other.ptr_;
    }

    bool operator!=(const basic_iterator& other) const {
      return !(*this == other);
    }

    operator typename Deque<T>::const_iterator() { return {ptr_, index_}; }

    friend basic_iterator operator+(const difference_type& difference,
                                    const basic_iterator& it) {
      return it + difference;
    }

    friend basic_iterator operator-(const difference_type& difference,
                                    const basic_iterator& it) {
      return it - difference;
    }

   private:
    T** ptr_;
    size_t index_;
  };

 public:
  iterator begin() {
    return iterator{external_array_ + first_block_, first_element_index_};
  }

  const_iterator begin() const {
    return const_iterator{external_array_ + first_block_, first_element_index_};
  }

  const_iterator cbegin() const {
    return const_iterator{external_array_ + first_block_, first_element_index_};
  }

  iterator end() {
    return iterator{external_array_ + last_block_, after_last_element_index_};
  }

  const_iterator end() const {
    return const_iterator{external_array_ + last_block_,
                          after_last_element_index_};
  }

  const_iterator cend() const {
    return const_iterator{external_array_ + last_block_,
                          after_last_element_index_};
  }

  reverse_iterator rbegin() { return reverse_iterator(end()); }

  const_reverse_iterator crbegin() const {
    return const_reverse_iterator(cend());
  }

  reverse_iterator rend() { return reverse_iterator(begin()); }

  const_reverse_iterator crend() const {
    return const_reverse_iterator(cbegin());
  }

 public:
  Deque()
      : amount_of_blocks_(DEFAULT_CAPACITY),
        first_block_(DEFAULT_CAPACITY / 2),
        first_element_index_(0),
        last_block_(DEFAULT_CAPACITY / 2),
        after_last_element_index_(0),
        external_array_(new T*[DEFAULT_CAPACITY]) {
    AllocateBlocks(external_array_, amount_of_blocks_);
  }

  Deque(const Deque& other)
      : amount_of_blocks_(other.amount_of_blocks_),
        first_block_(other.first_block_),
        first_element_index_(other.first_element_index_),
        last_block_(other.last_block_),
        after_last_element_index_(other.after_last_element_index_),
        external_array_(new T*[other.amount_of_blocks_]) {

    AllocateBlocks(external_array_, amount_of_blocks_);
    size_t block_index = 0;
    size_t deque_size = size();
    try {
      while (block_index < deque_size) {
        new (&(*this)[block_index]) T(other[block_index]);
        ++block_index;
      }
    } catch (...) {
      Destroy(block_index);
      Deallocate(amount_of_blocks_);
      throw;
    }
  }

  Deque(Deque&& other)
      : amount_of_blocks_(other.amount_of_blocks_),
        first_block_(other.first_block_),
        first_element_index_(other.first_element_index_),
        last_block_(other.last_block_),
        after_last_element_index_(other.after_last_element_index_),
        external_array_(other.external_array_) {
    other.external_array_ = nullptr;
    other.amount_of_blocks_ = 0;
    other.last_block_ = other.first_block_;
    other.first_element_index_ = 0;
    other.after_last_element_index_ = 0;
  }

  explicit Deque(size_t count)
      : amount_of_blocks_(
            std::max(DEFAULT_CAPACITY, count / BLOCK_SIZE * 3 + 1)),
        first_block_(amount_of_blocks_ / 2 - count / (BLOCK_SIZE * 2)),
        first_element_index_(0),
        last_block_(first_block_ + count / BLOCK_SIZE),
        after_last_element_index_(count % BLOCK_SIZE),
        external_array_(new T*[amount_of_blocks_]) {

    AllocateBlocks(external_array_, amount_of_blocks_);
    size_t block_index = 0;
    try {
      while (block_index < count) {
        new (&(*this)[block_index]) T();
        ++block_index;
      }
    } catch (...) {
      Destroy(block_index);
      Deallocate(amount_of_blocks_);
      throw;
    }
  }

  Deque(size_t count, const T& value)
      : amount_of_blocks_(
            std::max(DEFAULT_CAPACITY, count / BLOCK_SIZE * 3 + 1)),
        first_block_(amount_of_blocks_ / 2 - count / (BLOCK_SIZE * 2)),
        first_element_index_(0),
        last_block_(first_block_ + count / BLOCK_SIZE),
        after_last_element_index_(count % BLOCK_SIZE),
        external_array_(new T*[amount_of_blocks_]) {
    AllocateBlocks(external_array_, amount_of_blocks_);
    size_t block_index = 0;
    try {
      while (block_index < count) {
        new (&(*this)[block_index]) T(value);
        ++block_index;
      }
    } catch (...) {
      Destroy(block_index);
      Deallocate(amount_of_blocks_);
      throw;
    }
  }

  Deque(std::initializer_list<T> init_list) : Deque() {
    for (const auto& element : init_list) {
      push_back(element);
    }
  }

  ~Deque() {
    Destroy(size());
    Deallocate(amount_of_blocks_);
  }

  Deque& operator=(Deque other) {
    Swap(other);
    return *this;
  }

  size_t size() const {
    return (last_block_ - first_block_) * BLOCK_SIZE - first_element_index_ +
           after_last_element_index_;
  }

  T& operator[](size_t index) { return *(begin() + index); }

  const T& operator[](size_t index) const { return *(begin() + index); }

  T& at(size_t index) {
    if (index >= size()) {
      throw std::out_of_range("Index out of range");
    }
    return (*this)[index];
  }

  const T& at(size_t index) const {
    if (index >= size()) {
      throw std::out_of_range("Index out of range");
    }
    return (*this)[index];
  }

  void push_back(const T& value) {
    try {
      new (external_array_[last_block_] + after_last_element_index_) T(value);
    } catch (...) {
      throw;
    }
    if (last_block_ == amount_of_blocks_ - 1 &&
        after_last_element_index_ == BLOCK_SIZE - 1) {
      PushHelper(last_block_, after_last_element_index_);
    }
    ++after_last_element_index_;
    if (after_last_element_index_ == BLOCK_SIZE) {
      ++last_block_;
      after_last_element_index_ = 0;
    }
  }

  void push_back(T&& value) {
    new (external_array_[last_block_] + after_last_element_index_)
        T(std::move(value));
    if (last_block_ == amount_of_blocks_ - 1 &&
        after_last_element_index_ == BLOCK_SIZE - 1) {
      PushHelper(last_block_, after_last_element_index_);
    }
    ++after_last_element_index_;
    if (after_last_element_index_ == BLOCK_SIZE) {
      ++last_block_;
      after_last_element_index_ = 0;
    }
  }

  void pop_back() {
    (*this)[size() - 1].~T();
    if (after_last_element_index_ == 0) {
      after_last_element_index_ = BLOCK_SIZE - 1;
      --last_block_;
    } else {
      --after_last_element_index_;
    }
  }

  void push_front(const T& value) {
    size_t prev_first_element_index = first_element_index_ - 1;
    size_t prev_first_block = first_block_;
    if (first_element_index_ == 0) {
      prev_first_element_index = BLOCK_SIZE - 1;
      --prev_first_block;
    }
    new (external_array_[prev_first_block] + prev_first_element_index) T(value);
    if (prev_first_element_index == 0 && prev_first_block == 0) {
      PushHelper(prev_first_block, prev_first_element_index);
    }
    first_element_index_ = BLOCK_SIZE - first_element_index_;
    first_block_ -= (first_element_index_ / BLOCK_SIZE);
    first_element_index_ =
        (BLOCK_SIZE - 1 - (first_element_index_ % BLOCK_SIZE));
  }

  void push_front(T&& value) {
    size_t prev_first_element_index = first_element_index_ - 1;
    size_t prev_first_block = first_block_;
    if (first_element_index_ == 0) {
      prev_first_element_index = BLOCK_SIZE - 1;
      --prev_first_block;
    }
    new (external_array_[prev_first_block] + prev_first_element_index)
        T(std::move(value));
    if (prev_first_element_index == 0 && prev_first_block == 0) {
      PushHelper(prev_first_block, prev_first_element_index);
    }
    first_element_index_ = BLOCK_SIZE - first_element_index_;
    first_block_ -= (first_element_index_ / BLOCK_SIZE);
    first_element_index_ =
        (BLOCK_SIZE - 1 - (first_element_index_ % BLOCK_SIZE));
  }

  void pop_front() {
    (*this)[0].~T();
    if (first_element_index_ == BLOCK_SIZE - 1) {
      ++first_block_;
      first_element_index_ = 0;
    } else {
      ++first_element_index_;
    }
  }

  void insert(const iterator& it, const T& value) {
    if (size() == 0 || it == end()) {
      push_back(value);
      return;
    }
    T last_element = *(end() - 1);
    for (auto new_it = end() - 1; new_it != it; --new_it) {
      std::swap(*(new_it - 1), *new_it);
    }
    *it = value;
    push_back(last_element);
  }

  void insert(const iterator& it, const std::initializer_list<T>& values) {
    size_t i = 0;
    for (const auto& value : values) {
      insert(it + i, value);
      ++i;
    }
  }

  void erase(const iterator& it) {
    for (auto new_it = it; new_it != end() - 1; ++new_it) {
      std::swap(*new_it, *(new_it + 1));
    }
    pop_back();
  }

  bool operator==(const Deque& other) const {
    if (other.size() != size()) {
      return false;
    }
    for (size_t i = 0; i < size(); ++i) {
      if (other[i] != *(begin() + i)) {
        return false;
      }
    }
    return true;
  }

  bool operator==(const std::deque<T>& other) const {
    if (other.size() != size()) {
      return false;
    }
    for (size_t i = 0; i < size(); ++i) {
      if (other[i] != *(begin() + i)) {
        return false;
      }
    }
    return true;
  }

  bool empty() const { return size() == 0; }

  T& front() { return *(begin()); }

  const T& front() const { return *(begin()); }

  T& back() { return *(end() - 1); }

  const T& back() const { return *(end() - 1); }

 private:
  void Swap(Deque other) {
    std::swap(amount_of_blocks_, other.amount_of_blocks_);
    std::swap(first_block_, other.first_block_);
    std::swap(first_element_index_, other.first_element_index_);
    std::swap(last_block_, other.last_block_);
    std::swap(after_last_element_index_, other.after_last_element_index_);
    std::swap(external_array_, other.external_array_);
  }

  void PushHelper(size_t block, size_t element_index) {
    size_t new_amount_of_blocks = (amount_of_blocks_ + 1);
    T** new_external_array = new T*[new_amount_of_blocks * 3];
    try {
      AllocateBlocks(new_external_array, new_amount_of_blocks * 3);
    } catch (...) {
      (external_array_[block] + element_index)->~T();
      delete[] new_external_array;
      throw;
    }
    size_t tmp = last_block_ - first_block_;
    for (size_t i = 0; i <= tmp; ++i) {
      std::swap(external_array_[i + first_block_],
                new_external_array[i + new_amount_of_blocks]);
    }

    Deallocate(amount_of_blocks_);
    first_block_ = new_amount_of_blocks;
    last_block_ = first_block_ + tmp;
    amount_of_blocks_ = new_amount_of_blocks * 3;
    external_array_ = new_external_array;
  }

  void AllocateBlocks(T** array, size_t amount_of_blocks) {
    size_t block_index = 0;
    try {
      while (block_index < amount_of_blocks) {
        array[block_index] =
            reinterpret_cast<T*>(new char[BLOCK_SIZE * sizeof(T)]);
        ++block_index;
      }
    } catch (...) {
      Deallocate(block_index);
      throw;
    }
  }

  void Deallocate(size_t amount_of_blocks) {
    for (size_t i = 0; i < amount_of_blocks; ++i) {
      delete[] reinterpret_cast<char*>(external_array_[i]);
    }
    delete[] external_array_;
  }

  void Destroy(size_t count) {
    for (size_t i = 0; i < count; ++i) {
      pop_back();
    }
  }

 private:
  size_t amount_of_blocks_;

  size_t first_block_;
  size_t first_element_index_;

  size_t last_block_;
  size_t after_last_element_index_;

  T** external_array_;

  static constexpr size_t BLOCK_SIZE = 64;
  static constexpr size_t DEFAULT_CAPACITY = 64;
};
