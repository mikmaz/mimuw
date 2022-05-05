#include <stdio.h>
#include <stdlib.h>

#include "future.h"

int n;

static void* count_next(void *arg, size_t argsz __attribute__((unused)), size_t *retsz __attribute__((unused))) {
    int *ret = (int*)arg;
    if(ret[0] <= n) {
        ret[1] *= ret[0];
    }
    ret[0] += 3;

    return ret;
}


int main() {
    scanf("%d", &n);

    if(n < 3) {
        if(n < 0)
            return -1;

        if(n == 0)
            printf("1");
        else
            printf("%d", n);
        return 0;
    }

    int *res1 = malloc(sizeof(int)*2);
    int *res2 = malloc(sizeof(int)*2);
    int *res3 = malloc(sizeof(int)*2);

    res1[0] = 1;
    res2[0] = 2;
    res3[0] = 3;
    res1[1] = res2[1] = res3[1] = 1;

    int m = n/3 + 1;
    future_t tab1[m], tab2[m], tab3[m];

    thread_pool_t pool;
    if(thread_pool_init(&pool, 3) != 0)
        syserr("thread_pool_init");

    if(async(&pool, &tab1[0],
            (callable_t) {.function = count_next, .arg = res1, .argsz = sizeof(int)*2}) != 0)
        syserr("async");
    if(async(&pool, &tab2[0],
            (callable_t) {.function = count_next, .arg = res2, .argsz = sizeof(int)*2}) != 0)
        syserr("async");
    if(async(&pool, &tab3[0],
            (callable_t) {.function = count_next, .arg = res3, .argsz = sizeof(int)*2}) != 0)
        syserr("async");

    for(int i = 1; i < m; i++) {
        if(map(&pool, &tab1[i], &tab1[i-1], count_next) != 0)
            syserr("map");
        if(map(&pool, &tab2[i], &tab2[i-1], count_next) != 0)
            syserr("map");
        if(map(&pool, &tab3[i], &tab3[i-1], count_next) != 0)
            syserr("map");
    }

    await(&tab1[m-1]);
    await(&tab2[m-1]);
    await(&tab3[m-1]);

    int final = res1[1]*res2[1]*res3[1];
    printf("%d", final);

    for(int i = 0; i < m; i++) {
        future_destroy(&tab1[i]);
        future_destroy(&tab2[i]);
        future_destroy(&tab3[i]);
    }

    thread_pool_destroy(&pool);
    free(res1);
    free(res2);
    free(res3);
    return 0;
}
