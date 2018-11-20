#include <immintrin.h>
#include <mmintrin.h>

void print256_num(__m256i var);

void print_bits_64(uint64_t v);

int main_spliced(
    int argc,
    char **argv);

uint64_t process_chunk(
    uint8_t *in_buffer,
    size_t in_length,
    uint8_t *work_bits_of_d,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_a,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_z,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_q,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_b,       // Working buffer of minimum length ((in_length + 63) / 64)
    uint8_t *work_bits_of_e,       // Working buffer of minimum length ((in_length + 63) / 64)
    size_t *last_trailing_ones,
    size_t *quote_odds_carry,
    size_t *quote_evens_carry,
    uint64_t *quote_mask_carry,
    uint8_t *result_ibs,
    uint8_t *result_a,
    uint8_t *result_z);
