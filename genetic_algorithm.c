#include "genetic_algorithm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int read_input(sack_object **objects, int *object_count, int *sack_capacity, int *generations_count, int argc, char *argv[]) {
    FILE *fp;

    if (argc < 3) {
        fprintf(stderr, "Usage:\n\t./tema1 in_file generations_count\n");
        return 0;
    }

    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        return 0;
    }

    if (fscanf(fp, "%d %d", object_count, sack_capacity) < 2) {
        fclose(fp);
        return 0;
    }

    if (*object_count % 10) {
        fclose(fp);
        return 0;
    }

    sack_object *tmp_objects = (sack_object *)calloc(*object_count, sizeof(sack_object));

    for (int i = 0; i < *object_count; ++i) {
        if (fscanf(fp, "%d %d", &tmp_objects[i].profit, &tmp_objects[i].weight) < 2) {
            free(objects);
            fclose(fp);
            return 0;
        }
    }

    fclose(fp);

    *generations_count = (int)strtol(argv[2], NULL, 10);

    if (*generations_count == 0) {
        free(tmp_objects);

        return 0;
    }

    *objects = tmp_objects;

    return 1;
}

void print_objects(const sack_object *objects, int object_count) {
    for (int i = 0; i < object_count; ++i) {
        printf("%d %d\n", objects[i].weight, objects[i].profit);
    }
}

void print_generation(const individual *generation, int limit) {
    for (int i = 0; i < limit; ++i) {
        for (int j = 0; j < generation[i].chromosome_length; ++j) {
            printf("%d ", generation[i].chromosomes[j]);
        }

        printf("\n%d - %d\n", i, generation[i].fitness);
    }
}

void print_best_fitness(const individual *generation) {
    printf("%d\n", generation[0].fitness);
}

void compute_fitness_function(const sack_object *objects, individual *generation, int object_count, int sack_capacity, int inceput, int sfarsit) {
    int weight;
    int profit;

    for (int i = inceput; i < sfarsit; ++i) {
        weight = 0;
        profit = 0;

        for (int j = 0; j < generation[i].chromosome_length; ++j) {
            if (generation[i].chromosomes[j]) {
                weight += objects[j].weight;
                profit += objects[j].profit;
            }
        }

        generation[i].fitness = (weight <= sack_capacity) ? profit : 0;
    }
}

int cmpfunc(const void *a, const void *b) {
    int i;
    individual *first = (individual *)a;
    individual *second = (individual *)b;

    int res = second->fitness - first->fitness;  // decreasing by fitness
    if (res == 0) {
        int first_count = 0, second_count = 0;

        for (i = 0; i < first->chromosome_length && i < second->chromosome_length; ++i) {
            first_count += first->chromosomes[i];
            second_count += second->chromosomes[i];
        }

        res = first_count - second_count;  // increasing by number of objects in the sack
        if (res == 0) {
            return second->index - first->index;
        }
    }

    return res;
}

void mutate_bit_string_1(const individual *ind, int generation_index) {
    int i, mutation_size;
    int step = 1 + generation_index % (ind->chromosome_length - 2);

    if (ind->index % 2 == 0) {
        // for even-indexed individuals, mutate the first 40% chromosomes by a given step
        mutation_size = ind->chromosome_length * 4 / 10;
        for (i = 0; i < mutation_size; i += step) {
            ind->chromosomes[i] = 1 - ind->chromosomes[i];
        }
    } else {
        // for even-indexed individuals, mutate the last 80% chromosomes by a given step
        mutation_size = ind->chromosome_length * 8 / 10;
        for (i = ind->chromosome_length - mutation_size; i < ind->chromosome_length; i += step) {
            ind->chromosomes[i] = 1 - ind->chromosomes[i];
        }
    }
}

void mutate_bit_string_2(const individual *ind, int generation_index) {
    int step = 1 + generation_index % (ind->chromosome_length - 2);

    // mutate all chromosomes by a given step
    for (int i = 0; i < ind->chromosome_length; i += step) {
        ind->chromosomes[i] = 1 - ind->chromosomes[i];
    }
}

void crossover(individual *parent1, individual *child1, int generation_index) {
    individual *parent2 = parent1 + 1;
    individual *child2 = child1 + 1;
    int count = 1 + generation_index % parent1->chromosome_length;

    memcpy(child1->chromosomes, parent1->chromosomes, count * sizeof(int));
    memcpy(child1->chromosomes + count, parent2->chromosomes + count, (parent1->chromosome_length - count) * sizeof(int));

    memcpy(child2->chromosomes, parent2->chromosomes, count * sizeof(int));
    memcpy(child2->chromosomes + count, parent1->chromosomes + count, (parent1->chromosome_length - count) * sizeof(int));
}

void copy_individual(const individual *from, const individual *to) {
    memcpy(to->chromosomes, from->chromosomes, from->chromosome_length * sizeof(int));
}

void free_generation(individual *generation) {
    int i;

    for (i = 0; i < generation->chromosome_length; ++i) {
        free(generation[i].chromosomes);
        generation[i].chromosomes = NULL;
        generation[i].fitness = 0;
    }
}

individual *merge_sorted(individual *generatie, int nr_threaduri, int nr_obiecte) {
    int pozitie[5], max_pozitie[5], j = 0;

    // se obtin pozitiile de inceput si de final pentru fiecare interval sortat
    // din generatie
    for (int i = 0; i < nr_threaduri; i++) {
        pozitie[i] = i * (double)nr_obiecte / nr_threaduri;
        max_pozitie[i] = (i + 1) * (double)nr_obiecte / nr_threaduri;
    }

    // se aloca spatiu pentru noua generatie
    individual *generatie_sortata = calloc(nr_obiecte, sizeof(individual));

    for (j = 0; j <= nr_obiecte;) {
        individual cel_mai_bun;
        int index_cel_mai_bun = -1;
        for (int k = 0; k < nr_threaduri; k++) {
            // daca o portiune a ajuns cu pozitia la final atunci n-o mai luam
            // in considerare
            if (pozitie[k] != max_pozitie[k]) {
                // se compara cel mai bun individ curent (in functie de cmpfunc)
                // cu ceilalti indivizi de pe pozitiile curente in fiecare
                // portiune
                // daca nu s-a selectat inca un individ (index_cel_mai_bun e 0)
                // atunci se va selecta primul disponibil
                if (index_cel_mai_bun < 0 || cmpfunc(&cel_mai_bun, &generatie[pozitie[k]]) >= 0) {
                    index_cel_mai_bun = k;
                    cel_mai_bun = generatie[pozitie[k]];
                }
            }
        }
        // daca s-a ales un index bun, se adauga acel individ la noul vector
        if (index_cel_mai_bun != -1) {
            generatie_sortata[j] = generatie[pozitie[index_cel_mai_bun]];
            pozitie[index_cel_mai_bun] += 1;
        }
        // se face update la pozitia curenta din noul vector
        j++;
    }

    return generatie_sortata;
}

// functia pe care o ruleaza threadurile
void *run(void *arg) {
    Argumente *argumente = (Argumente *)arg;

    // preluarea argumentelor din structura
    individual *tmp = NULL;
    individual *generatie_curenta = argumente->generatie_curenta;
    individual *generatie_urmatoare = argumente->generatie_urmatoare;
    int id = argumente->id, nr_obiecte = argumente->nr_obiecte;
    int nr_generatii = argumente->nr_generatii, capacitate = argumente->capacitate;
    int nr_threaduri = argumente->nr_threaduri;
    const sack_object *obiecte = argumente->obiecte;
    int cursor, count;
    // bariera (necesara pentru sincronizare)
    pthread_barrier_t *bariera = argumente->bariera;

    int inceput, sfarsit;

    // calcularea indicilor de inceput si de sfarsit pentru fiecare thread
    inceput = id * (double)nr_obiecte / nr_threaduri;
    sfarsit = (id + 1) * (double)nr_obiecte / nr_threaduri;
    if (sfarsit > nr_obiecte)
        sfarsit = nr_obiecte;
    // set initial generation (composed of object_count individuals with a single item in the sack)
    // initializarea primei generatii
    for (int i = inceput; i < sfarsit; ++i) {
        generatie_curenta[i].fitness = 0;
        generatie_curenta[i].chromosomes = (int *)calloc(nr_obiecte, sizeof(int));
        generatie_curenta[i].chromosomes[i] = 1;
        generatie_curenta[i].index = i;
        generatie_curenta[i].chromosome_length = nr_obiecte;

        generatie_urmatoare[i].fitness = 0;
        generatie_urmatoare[i].chromosomes = (int *)calloc(nr_obiecte, sizeof(int));
        generatie_urmatoare[i].index = i;
        generatie_urmatoare[i].chromosome_length = nr_obiecte;
    }
    // se asteapta initializarea tuturor indivizilor din prima generatie de
    // catre fiecare thread
    pthread_barrier_wait(bariera);

    int inceput2, sfarsit2;
    // pentru fiecare generatie
    for (int k = 0; k < nr_generatii; ++k) {
        cursor = 0;

        // se calculeaza fitnesul fiecarui individ si fiecare thread sorteaza
        // intervalul corespunzator din generatia curenta
        compute_fitness_function(obiecte, generatie_curenta, nr_obiecte, capacitate, inceput, sfarsit);
        // se asteapta calcularea fitness-ului
        pthread_barrier_wait(bariera);
        qsort(generatie_curenta + inceput, sfarsit - inceput, sizeof(individual), cmpfunc);
        // si sortarea de catre fiecare thread, de asemenea
        pthread_barrier_wait(bariera);
        // dupa ce au fost realizate toate sortarile pe bucati, se face un
        // "merge" al bucatilor sortate din generatie
        // merge-ul este realizat de catre un singur thread si are nevoie de
        // toate intervalele din vector sa fie sortate
        if (id == 0) {
            // se obtine generatia sortata
            individual *generatie_sortata = merge_sorted(generatie_curenta, nr_threaduri, nr_obiecte);
            // se actualizeaza generatia curenta cu indivizii sortati
            for (int j = 0; j < nr_obiecte; j++)
                generatie_curenta[j] = generatie_sortata[j];

            // se elibereaza spatiul ocupat
            free(generatie_sortata);
        }
        // se asteapta sa se faca merge-ul
        pthread_barrier_wait(bariera);
        count = nr_obiecte * 3 / 10;
        inceput2 = id * (double)count / nr_threaduri;
        sfarsit2 = (id + 1) * (double)count / nr_threaduri;

        if (sfarsit2 > count)
            sfarsit2 = count;
        // dupa urmeaza cateva operatii pe acea generatie
        // pastrarea elitei (se pastreaza primii 30% din indivizi)
        for (int i = inceput2; i < sfarsit2; ++i) {
            copy_individual(generatie_curenta + i, generatie_urmatoare + i);
        }
        cursor = count;
        count = nr_obiecte * 2 / 10;
        inceput2 = id * (double)count / nr_threaduri;
        sfarsit2 = (id + 1) * (double)count / nr_threaduri;

        if (sfarsit2 > count)
            sfarsit2 = count;

        // mutate first 20% children with the first version of bit string mutation
        // primii 20% din indivizi sufera o mutatie cu prima functie de bit mutation
        for (int i = inceput2; i < sfarsit2; ++i) {
            copy_individual(generatie_curenta + i, generatie_urmatoare + cursor + i);
            mutate_bit_string_1(generatie_urmatoare + cursor + i, k);
        }
        cursor += count;
        count = nr_obiecte * 2 / 10;
        inceput2 = id * (double)count / nr_threaduri;
        sfarsit2 = (id + 1) * (double)count / nr_threaduri;

        if (sfarsit2 > count)
            sfarsit2 = count;

        // mutate next 20% children with the second version of bit string mutation
        // urmatorii 20% sufera o mutatie cu a doua versiune de bit mutation
        for (int i = inceput2; i < sfarsit2; ++i) {
            copy_individual(generatie_curenta + i + count, generatie_urmatoare + cursor + i);
            mutate_bit_string_2(generatie_urmatoare + cursor + i, k);
        }
        cursor += count;

        // crossover first 30% parents with one-point crossover
        // (if there is an odd number of parents, the last one is kept as such)
        // se face operatia de crossover pe un singur thread, nu s-a putut
        // paraleliza
        pthread_barrier_wait(bariera);
        if (id == 0) {
            count = nr_obiecte * 3 / 10;

            if (count % 2 == 1) {
                copy_individual(generatie_curenta + nr_obiecte - 1, generatie_urmatoare + cursor + count - 1);
                count--;
            }

            for (int i = 0; i < count; i += 2) {
                crossover(generatie_curenta + i, generatie_urmatoare + cursor + i, k);
            }
        }
        pthread_barrier_wait(bariera);
        // se trece la o noua generatie
        tmp = generatie_curenta;
        generatie_curenta = generatie_urmatoare;
        generatie_urmatoare = tmp;

        for (int i = inceput; i < sfarsit; ++i) {
            generatie_curenta[i].index = i;
        }

        // se afiseaza fitness-ul cel mai bun pe un singur thread daca
        // generatia e divizibila cu 5
        if (k % 5 == 0) {
            if (id == 0)
                print_best_fitness(generatie_curenta);
        }

        pthread_barrier_wait(bariera);
    }

    // pentru ultima generatie se calculeaza fitness-ul si se sorteaza
    // paralelizat, apoi merge
    compute_fitness_function(obiecte, generatie_curenta, nr_obiecte, capacitate, inceput, sfarsit);
    pthread_barrier_wait(bariera);
    qsort(generatie_curenta + inceput, sfarsit - inceput, sizeof(individual), cmpfunc);
    pthread_barrier_wait(bariera);
    if (id == 0) {
        individual *generatie_sortata = merge_sorted(generatie_curenta, nr_threaduri, nr_obiecte);
        for (int j = 0; j < nr_obiecte; j++)
            generatie_curenta[j] = generatie_sortata[j];

        free(generatie_sortata);
    }

    // se afiseaza fitness-ul cel mai bun de la ultima generatie
    pthread_barrier_wait(bariera);
    if (id == 0) {
        print_best_fitness(generatie_curenta);
    }
    return NULL;
}

void run_genetic_algorithm(const sack_object *objects, int object_count,
                           int generations_count, int sack_capacity, int nr_threaduri) {
    individual *current_generation = (individual *)calloc(object_count, sizeof(individual));
    individual *next_generation = (individual *)calloc(object_count, sizeof(individual));

    pthread_barrier_t bariera;

    pthread_t threads[nr_threaduri];
    Argumente argumente[nr_threaduri];

    // se trimit toate argumentele neceasarii rularii threadului
    pthread_barrier_init(&bariera, NULL, nr_threaduri);
    for (int i = 0; i < nr_threaduri; i++) {
        argumente[i].bariera = &bariera;
        argumente[i].capacitate = sack_capacity;
        argumente[i].generatie_curenta = current_generation;
        argumente[i].generatie_urmatoare = next_generation;
        argumente[i].id = i;
        argumente[i].nr_generatii = generations_count;
        argumente[i].nr_obiecte = object_count;
        argumente[i].nr_threaduri = nr_threaduri;
        argumente[i].obiecte = objects;

        // se creeaza threadurile si se verifica rezultatul creari
        int rezultat = pthread_create(&threads[i], NULL, run, &argumente[i]);

        if (rezultat) {
            printf("Eroare la creare thread %d\n", i);
            exit(1);
        }
    }

    for (int i = 0; i < nr_threaduri; i++) {
        int rezultat = pthread_join(threads[i], NULL);

        if (rezultat) {
            printf("Eroare la asteptare thread %d\n", i);
            exit(1);
        }
    }

    // free resources for old generation
    free_generation(current_generation);
    free_generation(next_generation);

    // free resources
    free(current_generation);
    free(next_generation);
    pthread_barrier_destroy(&bariera);
}