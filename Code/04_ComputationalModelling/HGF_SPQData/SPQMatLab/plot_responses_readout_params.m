%% correlate simulated responses with real data and read out individually fitted parameters
load est_spq_reverse_level4.mat % load in the estimated model here

muahat = NaN(length(est(1).y),length(est));
bw_real = NaN(length(est(1).y),length(est));
bw_sim = NaN(length(est(1).y),length(est));
mean_real = NaN(length(est(1).y),length(est));
mean_sim = NaN(length(est(1).y),length(est));

for sub = 1:length(est)

    fa  = est(sub).y(:,2);
    bw_real(:,sub) = fa; 
    
    fafa  = est(sub).y(:,1);
    mean_real(:,sub) = fafa; 
    
    ta  = est(sub).optim.yhat(:,2);
    bw_sim(:,sub) = ta; 
    
    tata  = est(sub).optim.yhat(:,1);
    mean_sim(:,sub) = tata; 
    
    bla             = est(sub).traj.muahat(:,1);
    muahat(:, sub)  = bla;
    
    % store the individual parameters
    mux(sub)   = est(sub).p_prc.mux_0(1);  % initial belief about mean (close to first input)
    saa_0(sub) = est(sub).p_prc.saa_0(1);  % initial uncertainity about SD, should actually be fixed
    kax(sub)   = est(sub).p_prc.kax;       % kappa for mean learning (sometimes fixed to 0)
    kaa(sub)   = est(sub).p_prc.kaa;       % kappa for SD learning (sometimes fixed to 0)
    be1(sub)   = est(sub).p_obs.be1;       % free factor for use of noise belief
    zem(sub)   = est(sub).p_obs.zem;       % noise in mean learning
    zes(sub)   = est(sub).p_obs.zes;       % noise in SD learning
    
    clear fa bla ta fafa tata 
end

figure; plot(median(bw_sim,2)); hold on; plot(median(bw_real,2)); title("Median across all: real beam width (red) and simulated bw (blue)");  
figure; plot(median(mean_sim,2)); hold on; plot(median(mean_real,2)); title("Median across all: real prediction (red) and simulated prediction (blue)");  
       