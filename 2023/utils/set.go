package utils

import "fmt"

type Set[T comparable] struct {
	values map[T]bool
}

func NewSet[T comparable](initValues ...T) *Set[T] {
	s := &Set[T]{
		make(map[T]bool),
	}
	for _, value := range initValues {
		s.Add(value)
	}
	return s
}

func NewSetFromSlice[T comparable](slice []T) *Set[T] {
	return NewSet[T](slice...)
}

func (s *Set[T]) Copy() *Set[T] {
	values := make(map[T]bool)
	for value := range s.values {
		values[value] = true
	}
	return &Set[T]{values}
}

func (s *Set[T]) Add(value T) {
	s.values[value] = true
}

func (s *Set[T]) Contains(value T) bool {
	return s.values[value]
}

func (s *Set[T]) IsContainedIn(other *Set[T]) bool {
	for value := range s.values {
		if !other.Contains(value) {
			return false
		}
	}
	return true
}

func (s *Set[T]) Equals(other *Set[T]) bool {
	return s.IsContainedIn(other) && other.IsContainedIn(s)
}

func (s *Set[T]) Remove(value T) {
	delete(s.values, value)
}

func (s *Set[T]) Size() int {
	return len(s.values)
}

// TODO: make this in place
func (s *Set[T]) Intersect(other *Set[T]) *Set[T] {
	intersection := NewSet[T]()

	for value := range s.values {
		if other.Contains(value) {
			intersection.Add(value)
		}
	}

	return intersection
}

func (s *Set[T]) Union(other *Set[T]) *Set[T] {
	union := NewSet[T]()

	for value := range s.values {
		union.Add(value)
	}

	for value := range other.values {
		union.Add(value)
	}

	return union
}

func (s *Set[T]) InPlaceUnion(other *Set[T]) {
	for value := range other.values {
		s.Add(value)
	}
}

func (s *Set[T]) Sub(other *Set[T]) *Set[T] {
	sub := NewSet[T]()

	for value := range s.values {
		if !other.Contains(value) {
			sub.Add(value)
		}
	}

	return sub
}

func (s *Set[T]) IsEmpty() bool {
	return s.Size() == 0
}

func (s *Set[T]) IsNotEmpty() bool {
	return !s.IsEmpty()
}

func (s *Set[T]) Values() []T {
	values := make([]T, 0, len(s.values))
	for value := range s.values {
		values = append(values, value)
	}
	return values
}

func (s *Set[T]) String() string {
	return fmt.Sprintf("Set%v", s.Values())
}
