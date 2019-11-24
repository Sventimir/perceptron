/* Originally written by Głupia Nauka. This is only included as a reference
   program. It yields exactly the same results as the Haskell implementation,
   only less precise due to Doubles used instead of Floats. */
#include <stdio.h>
#include <vector>
#include <string.h>
#include <math.h>
#include <assert.h>

typedef struct _sample{
    float temp;
    float season;
    float decision;
} sample;


inline float max(float a, float b){
    return a > b ? a :  b;
}

inline float min(float a, float b){
    return a < b ? a :  b;
}



class Perceptron{
public:
    static constexpr float WEIGHT_SPAN = 1;
    static constexpr float LEARNING_RATE = 0.08;
    int inputSize;
    std::vector<float> inputs;
    std::vector<float> weights;
    float value;
    float sum;
    float error;

    Perceptron(int iSize, int bias):inputSize(iSize+bias),inputs(iSize+bias,0),value(0), sum(0), error(0){

        this->weights.push_back(-1);
        this->weights.push_back(1);
        this->weights.push_back(0);
        if (bias == 1){
            inputs.back() = -1;
        }
    }

    float calculateValue(){
        this->sum = 0;
        for(int i = 0; i < this->inputSize; ++i){
            this->sum += this->inputs[i] * this->weights[i];
        }
        this->value = this->activation(this->sum);
        return this->value;
    }

    void setInput(int idx, float val){
        assert(idx < inputSize);
        this->inputs[idx] = val;
    }

    void train(float expectedVal){
        //we use chain role to calculate derivative
        float derivativeOfLossFunction = this->value - expectedVal; //it is a derivative of square error function for a (a - b)^2 ==> a^2 -2ab + b^2    derivative    2a - 2b  (2 is a constant so for convinience we can devided error function)

        float derivativeOfPerceptron = derivativeOfLossFunction * this->derivative(this->sum);

        for(int i = 0; i < this->inputSize; ++i){
            //CHAIN RULE CALCULATION
            float derivativeOfWeight =  derivativeOfPerceptron * this->inputs[i];
            //NOW WE KNOW GRADIENT, SO WE CAN UPDATE WEIGHT
            this->weights[i] -= LEARNING_RATE * derivativeOfWeight;
        }
    }

    float calculateError(float expectedVal){
        this->error = (expectedVal - this->value) * (expectedVal - this->value) / 2;//this 2 is for remove constant from derivative
        return this->error;
    }

    float activation(float value){
        return 1.0/(1.0 + exp(-value));

    }
    float derivative(float value){
        float f = 1.0/(1.0 + exp(-value));
        return f * (1.0 - f);
    }

    void print(){
        for(int i = 0; i < this->inputSize; ++i){
            printf("%f ",this->weights[i]);
        }
        printf("\n");
    }

    virtual ~Perceptron(){}
};





void readDataSet(std::vector<sample> & trainingSet, const char * path){
    FILE * f = fopen(path, "r");
    int temp;
    char season[12];
    int decision;
    while(fscanf(f, "%d %s %d", &temp, season, &decision) != EOF){
        float numSeason;
        if (strcmp(season,"winter") == 0){
            numSeason = 1;
        } else if (strcmp(season,"autumn") == 0){
            numSeason = 2;
        } else if (strcmp(season,"spring") == 0){
            numSeason = 3;
        } else if (strcmp(season,"summer") == 0){
            numSeason = 4;
        }
        trainingSet.push_back((sample){(float)temp,numSeason,(float)decision});
    }
    fclose(f);
}


void readDataSet2(std::vector<sample> & trainingSet, const char * path){
    FILE * f = fopen(path, "r");
    int temp;
    char season[12];
    int decision;
    while(fscanf(f, "%d %s %d", &temp, season, &decision) != EOF){
        float numSeason;
        if (strcmp(season,"winter") == 0){
            numSeason = 4;
        } else if (strcmp(season,"autumn") == 0){
            numSeason = 3;
        } else if (strcmp(season,"spring") == 0){
            numSeason = 2;
        } else if (strcmp(season,"summer") == 0){
            numSeason = 1;
        }
        trainingSet.push_back((sample){(float)temp,numSeason,(float)decision});
    }
    fclose(f);
}

void normaliseDataSet(std::vector<sample> & trainingSet){//Normalization to interval   <0,1>
    float maxTemp = -1000;
    float minTemp = 1000;
    float maxSeason = -1000;
    float minSeason = 1000;
    for(int i = 0 ; i < (int)trainingSet.size(); ++i){
        maxTemp = max(trainingSet[i].temp, maxTemp);
        minTemp = min(trainingSet[i].temp, minTemp);
        maxSeason = max(trainingSet[i].season, maxSeason);
        minSeason = min(trainingSet[i].season, minSeason);
    }
    for(int i = 0 ; i < (int)trainingSet.size(); ++i){
        trainingSet[i].temp = (trainingSet[i].temp - minTemp) / (maxTemp - minTemp);
        trainingSet[i].season = (trainingSet[i].season - minSeason) / (maxSeason - minSeason);
    }
}

void printDataSet(std::vector<sample> & trainingSet){
    printf("DATA\n");
    for(int i = 0 ; i < (int)trainingSet.size(); ++i){
        printf("%f %f      %f\n",trainingSet[i].temp, trainingSet[i].season, trainingSet[i].decision);
    }
}

int NUMBER_OF_EPOCH = 20;

int main(){

    std::vector<sample> trainingSet;
    readDataSet(trainingSet, "trainingData.txt");
    normaliseDataSet(trainingSet);
    printDataSet(trainingSet);

    Perceptron per(2,1);
    for(int i = 0; i < NUMBER_OF_EPOCH; ++i){
        float accError = 0;
        float hit = 0;
        for(int j = 0; j < (int)trainingSet.size(); ++j){
            per.setInput(0, trainingSet[j].temp);
            per.setInput(1, trainingSet[j].season);
            float result = per.calculateValue();
            if ((trainingSet[j].decision == 1 &&  result > 0.5 ) ||
                    (trainingSet[j].decision == 0 &&  result < 0.5 )){
                ++hit;
            }
            accError += per.calculateError(trainingSet[j].decision);
            per.train(trainingSet[j].decision);
        }
        printf("TOTAL ERROR AFTER %d  percent error:%f  square error:%f\n", i, hit/trainingSet.size(), accError);
        per.print();
    }

    return 0;
}
