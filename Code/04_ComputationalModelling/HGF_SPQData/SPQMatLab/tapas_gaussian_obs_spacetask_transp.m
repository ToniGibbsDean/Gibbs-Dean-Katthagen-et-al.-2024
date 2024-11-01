function [pvec, pstruct] = tapas_gaussian_obs_spacetask_transp(r, ptrans)
% --------------------------------------------------------------------------------------------------
% Copyright (C) 2012-2013 Christoph Mathys, TNU, UZH & ETHZ
%
% This file is part of the HGF toolbox, which is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version). For further details, see the file
% COPYING or <http://www.gnu.org/licenses/>.

pvec    = NaN(1,length(ptrans));
pstruct = struct;

% pvec(1)    = exp(ptrans(1));         % ze
% pstruct.ze = pvec(1);

% if r.c_obs.dyn_noise == 0
pvec(1)     = exp(ptrans(1));         % beta1
pstruct.be1 = pvec(1);
pvec(2)     = ptrans(2);         % zeta
pstruct.zem  = pvec(2);
pvec(3)     = exp(ptrans(3));         % zeta
pstruct.zes  = pvec(3);



% elseif r.c_obs.dyn_noise == 1
% pvec(1) = tapas_sgm(ptrans(1), 1); % for standard model with (free) noise parameter, for dynamic: log(1)
% pstruct.ze = pvec(1);
% end


return;