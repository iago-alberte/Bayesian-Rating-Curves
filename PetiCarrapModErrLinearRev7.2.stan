functions {
  vector k2_equation(vector k2, vector theta, data array[] real x_r, data array[] int x_i){
    vector[1] f;
    real a1 = theta[1];
    real a2 = theta[2];
    real b1 = theta[3];
    real b2 = theta[4];
    real c1 = theta[5];
    real c2 = theta[6];
    f[1] = a1 * pow(k2[1] - b1, c1) - a2 * pow(k2[1] - b2, c2);
    return f;
  }
}
data {
	int<lower=0> N;
	int<lower=0> K;
	vector[N] h;
	vector[N] Q;
	vector[N] k;
	vector[N] mi;
	real x_r[0];  // Vetor real vazio
  	int x_i[0];   // Vetor int vazio
}
parameters {
	real<lower=0> a1;
	real<lower=1.3, upper=1.9> c1;
	real<lower=0> a2;
	real<lower=1.3, upper=1.9> c2;
	real<lower=0> a3;
	real<lower=1.3, upper=1.9> c3;
	real<lower=0> a4;
	real<lower=1.3, upper=1.9> c4;
	real gamma1;
	real gamma2;
	vector[K] k1;
	vector[K] k2;
	vector[K] k3;
	real b4;
}
transformed parameters{
	vector[K] b1;
	vector[K] b2;
	vector[K] b3;
	vector[K] k4;
	{
		vector[1] y_guess;
    	vector[1] solution;
    	real theta[6];  // Agora é array[] real, não vector

	for(i in 1:K){
		b1[i] = k1[i];
		b2[i] = k2[i] - ((a1/a2)*(k2[i]-b1[i])^c1)^(1/c2);
		b3[i] = k3[i] - ((a2/a3)*(k3[i]-b2[i])^c2)^(1/c3);
	}
	
	//Loop para obter a convergência entre os tramos 3 e 4
    for (i in 1:K) {
      theta[1] = a3;
      theta[2] = a4;
      theta[3] = b3[i];
      theta[4] = b4;
      theta[5] = c3;
      theta[6] = c4;
      y_guess[1] = k3[i]+1.0;
      //print("θ k4 = [", theta[1], ", ", theta[2], ", ", theta[3], ", ", theta[4], ", ", theta[5], ", ", theta[6], "], y_guess = ", y_guess[1]);
      solution = algebra_solver(k2_equation, y_guess, to_vector(theta), x_r, x_i,
                              0.0001, 0.5, 1e4);
      //print("solution[1] para i = ", i, ": ", solution[1]);
      if (!is_nan(solution[1])) {
        k4[i] = solution[1];
        } else {
            reject("Solução inválida para k4_equation no índice ", i);
            }
    }
    for (i in 1:K) {
        if (!(k1[i] < k2[i] && k2[i] < k3[i] && k3[i] < k4[i])) {
            reject("Ordem inválida dos pontos de quebra: k1[", i, "] = ", k1[i],
                ", k2[", i, "] = ", k2[i],
                ", k3[", i, "] = ", k3[i],
				", k4[", i, "] = ", k4[i]);
        }
        if (k1[i] < b1[i] || k2[i] < b2[i] || k3[i] < b3[i] ||  k4[i] < b4) {
            reject("k abaixo do limite de b: k1[", i, "] = ", k1[i],
                ", b1[", i, "] = ", b1[i],
                "; k2[", i, "] = ", k2[i],
                ", b2[", i, "] = ", b2[i],
                "; k3[", i, "] = ", k3[i],
				", b3[", i, "] = ", b3[i],
				"; k4[", i, "] = ", k4[i],
                ", b4 = ", b4);
        }
	}
	}
}
model {
	vector[N] mu;
    vector[N] sigma;
    //vector[N] erro;
	a1 ~ normal(40,10);
	k1[1] ~ uniform(-0.5, 2.5);
	c1 ~ normal(1.5, 0.025);
	a2 ~ normal(20,5);
	k2[1] ~ uniform(1, 5.5);
	c2 ~ normal(1.67, 0.025);
	a3 ~ normal(20,5);
	k3[1] ~ normal(6.0, 0.8);
	c3 ~ normal(1.67, 0.25);
    	a4 ~ normal(22, 0.5);
	b4 ~ normal(0, 0.30);
	c4 ~ lognormal(log(1.75), 0.02);
	gamma1 ~ uniform(0.0001,0.3);
	gamma2 ~ uniform(0.0001,1);
	for(i in 2:K){
		k1[i] ~ normal(k1[i-1],0.5);
		k2[i] ~ normal(k2[i-1],0.5);
        k3[i] ~ normal(k3[i-1],0.5);
	}	
	for(n in 1:N){
		for(i in 1:K){
				if(k[n] == i){
                    if(h[n] <= k2[i] && h[n] > k1[i]){
                        real base = h[n] - b1[i];
                        if (base > 0)
                            mu[n] = a1 * pow(base, c1);
                        else
                            reject("Valor inválido de base para pow() no trecho 1, n = ", n);
                    }
                    else if(h[n]> k2[i] && h[n]<=k3[i]){
                        real base = h[n] - b2[i];
                        if (base > 0)
                            mu[n] = a2 * pow(base, c2);
                        else
                            reject("Valor inválido de base para pow() no trecho 2, n = ", n);
                    }
                    else if(h[n]> k3[i] && h[n]<=k4[i]){
                        real base = h[n] - b3[i];
                        if (base > 0)
                            mu[n] = a3 * pow(base, c3);
                        else
                            reject("Valor inválido de base para pow() no trecho 3, n = ", n);
                    }					
                    else if(h[n]> k4[i]){
                        real base = h[n] - b4;
                        if (base > 0)
                            mu[n] = a4 * pow(base, c4);
                        else
                        reject("Valor inválido de base para pow() no trecho 4, n = ", n);
                    }
                    sigma[n] = gamma1 + gamma2 * mu[n];
                    //print("n = ", n,", i = ", i ,", h[n] = ", h[n],", Q[n] = ", Q[n] , ", b1[i] = ", b1[i],", b2[i] = ", b2[i],", b3[i] = ", b3[i] ,", b4 = ", b4 ,", k1[i] = ", k1[i],", k2[i] = ", k2[i] ,", k3[i] = ",k3[i]," k4[i] = ", k4[i] , ", mu[n] = ", mu[n], ", sigma[n] = ", sigma[n], ", mi[n] = ", mi[n]);
                    Q[n] ~ normal(mu[n], sqrt(square(sigma[n]) + square(mi[n])));
				}
		}
	}
}
