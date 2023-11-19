function varargout=arrow(varargin)

% warning! it is assumed that axes limits and paper position/size don't change any more !
%
% draws a 2D arrow between points X1 and X2.
%

X1=varargin{1};
X2=varargin{2};

% default options
L=12;
linewidth=1;
facecolor='k';
edgecolor='k';
ends=[true true];

% read options
varargin=varargin(3:end);
while ~isempty(varargin)
	if ~ischar(varargin{1})
		error('expected property name');
	else
		switch varargin{1}
			case 'length'
				L=varargin{2};
			case 'linewidth'
				linewidth=varargin{2};
			case 'facecolor'
				facecolor=varargin{2};
			case 'ends'
				switch varargin{2}
					case 'both'
						ends=[true true];
					case 'begin'
						ends=[true false];
					case 'end'
						ends=[false true];
				end
			case 'edgecolor'
				edgecolor=varargin{2};
			case default
				error('unknown arrow property')
		end
	end
	varargin=varargin(3:end);
end

% handles
h=nan(3,1);

% get transformation properties
pu=get(gcf,'paperunits');
set(gcf,'paperunits','inches');
ppos=get(gcf,'paperposition');
set(gcf,'paperunits',pu);
apos=get(gca,'position');

% x- and y-scale in units/point
xscale=diff(xlim)/(ppos(3)*apos(3)*72);
yscale=diff(ylim)/(ppos(4)*apos(4)*72);
S=diag([xscale,yscale]);

% triangle head in normal point space
Xh=[0 -1 -1;0 .3 -.3]*L;

% rotation matrix
theta=atan2(xscale*(X2(2)-X1(2)),yscale*(X2(1)-X1(1)));
R=[cos(theta) -sin(theta);sin(theta) cos(theta)];

% apply transformations
Xh=S*R*Xh;

% draw line
h(1)=line([X1(1) X2(1)],[X1(2),X2(2)],'color',edgecolor,'linewidth',linewidth);

% draw heads
if ends(1)
	h(end+1)=patch(X1(1)-Xh(1,:),X1(2)-Xh(2,:),'facecolor',facecolor,'edgecolor',edgecolor,'linewidth',linewidth);
end
if ends(2)
	h(end+1)=patch(X2(1)+Xh(1,:),X2(2)+Xh(2,:),'facecolor',facecolor,'edgecolor',edgecolor,'linewidth',linewidth);
end

% return handles
if (nargout>0)
	varargout{1}=h;
end
