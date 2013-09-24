/**
@file
@author Sampsa "Tuplanolla" Kiiskinen
**/

#include "arity.h" // enum arity

#include <stddef.h> // size_t

int arity_to_integral
(size_t* const result, enum arity const arity) {
	switch (arity) {
	case ARITY_NILADIC:
		*result = 0;
		return 0;
	case ARITY_MONADIC:
		*result = 1;
		return 0;
	case ARITY_DYADIC:
		*result = 2;
		return 0;
	case ARITY_TRIADIC:
		*result = 3;
		return 0;
	case ARITY_TETRADIC:
		*result = 4;
		return 0;
	case ARITY_PENTADIC:
		*result = 5;
		return 0;
	case ARITY_HEXADIC:
		*result = 6;
		return 0;
	case ARITY_HEPTADIC:
		*result = 7;
		return 0;
	case ARITY_OCTADIC:
		*result = 8;
		return 0;
	case ARITY_NONADIC:
		*result = 9;
		return 0;
	case ARITY_DECADIC:
		*result = 10;
		return 0;
	}
	return -1;
}

int arity_from_integral
(enum arity* const result, size_t const integral) {
	switch (integral) {
	case 0:
		*result = ARITY_NILADIC;
		return 0;
	case 1:
		*result = ARITY_MONADIC;
		return 0;
	case 2:
		*result = ARITY_DYADIC;
		return 0;
	case 3:
		*result = ARITY_TRIADIC;
		return 0;
	case 4:
		*result = ARITY_TETRADIC;
		return 0;
	case 5:
		*result = ARITY_PENTADIC;
		return 0;
	case 6:
		*result = ARITY_HEXADIC;
		return 0;
	case 7:
		*result = ARITY_HEPTADIC;
		return 0;
	case 8:
		*result = ARITY_OCTADIC;
		return 0;
	case 9:
		*result = ARITY_NONADIC;
		return 0;
	case 10:
		*result = ARITY_DECADIC;
		return 0;
	}
	return -1;
}
