#include <iostream>
#include <algorithm>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>
#define rep(i,a,b) for(int i=a,_b=b;i<=_b;++i)
#define per(i,a,b) for(int i=a,_b=b;i>=_b;--i)
#define For(i,a) for(int i=0,_a=a;i<_a;++i)
using namespace std;

const int wasteland_size=10;
const int neutral_size=2;
const int initial_armies=2;
const int max_super_regions=100;
const int max_regions=1000;

struct Super_region{
	int bonus,player1,player2,neutral,belong;
	double rating;
	vector<int> servant;
}super_regions[max_super_regions];

struct Region{
	int master,armies,belong,visible,near0,near2,transrating;
	double rating;
}regions[max_regions];

struct attack_transfer{
	int u,v,l;
	attack_transfer(int u,int v,int l):u(u),v(v),l(l){};
};

int N,M,timebank,time_per_move,max_rounds,income,cnt_list,place_armies[max_regions],bfs_q[max_regions],bfs_f[max_regions];
int neighbors[max_regions][max_regions];
string player1,player2;
vector<int> starting_regions,attack_rank;
vector<attack_transfer> attack_list,transfer_list;


inline double sqr(double x){return x*x;}

void renew_map(){
	rep(i,1,N) regions[i].rating=regions[i].near2=regions[i].near0=0;
	rep(i,1,N) rep(j,1,N) if (neighbors[i][j] && regions[j].belong==2) regions[i].near2=1;
	rep(i,1,N) rep(j,1,N) if (neighbors[i][j] && regions[j].belong==0) regions[i].near0=1;

	rep(i,1,M){
		// bonus/max(p2*0.3+p0-p1,1)				//p1:visible to player2
		int flag=1,cntp1=0,cntp2=0,cntne=0;

		super_regions[i].player1=0;
		super_regions[i].player2=0;
		super_regions[i].neutral=0;
		for(auto& v:super_regions[i].servant){
			if (regions[v].belong==1) super_regions[i].player1+=regions[v].armies,cntp1++;
			if (regions[v].belong==2) super_regions[i].player2+=regions[v].armies,cntp2++; //if unvisible +=1
			if (regions[v].belong==0){
				cntne++;
				if (regions[v].visible || regions[v].armies>neutral_size)
					super_regions[i].neutral+=regions[v].armies;
				else super_regions[i].neutral+=neutral_size;
			}

			if (regions[v].belong!=2 && regions[v].visible==1) flag=0;
		}
		super_regions[i].rating=(double)super_regions[i].bonus/max(1.0,sqr(0.3*super_regions[i].player2+super_regions[i].neutral));
		//super_regions[i].rating=(double)super_regions[i].bonus/max(0.01,pow(super_regions[i].neutral-max(0,super_regions[i].player1-super_regions[i].player2),1.5));

		if (flag && super_regions[i].bonus>0) for(auto& v:super_regions[i].servant) regions[v].rating=100;
		if (cntp2==1 && cntne==0 && super_regions[i].bonus>0) for(auto& v:super_regions[i].servant) regions[v].rating=102;
		if (cntp2==0 && cntne==1 && super_regions[i].bonus>0) for(auto& v:super_regions[i].servant) regions[v].rating=102;

		if (cntp1==0 && cntne==0) super_regions[i].belong=2;
		else if (cntp2==0 && cntne==0) super_regions[i].belong=1;
		else super_regions[i].belong=0;
	}

	int can_see_player2=0;
	rep(i,1,N) if (regions[i].visible && regions[i].belong==2) can_see_player2=1;
	rep(i,1,N) if (regions[i].belong==2 /*有敌人*/ || (regions[i].belong==0 && regions[i].armies>neutral_size) /*有废土*/ || (can_see_player2 && super_regions[regions[i].master].neutral>5) /*前线打架且要开的地大于3块*/){
		int u=regions[i].master;
		for(auto& v:super_regions[u].servant)
			if (regions[v].belong==0) regions[v].rating=-100;
	}
	// rep(i,1,N) if (regions[i].visible && regions[i].belong!=1) cout<<"i="<<i<<": "<<regions[i].rating<<endl;

	rep(i,1,N) {
		int u=regions[i].master;
		if (regions[i].belong==2)
			regions[i].rating+=super_regions[u].rating*(1.0+1.0/regions[i].armies);
		else if (regions[i].belong==0)
			regions[i].rating+=super_regions[u].rating*(1.0+1.0/regions[i].armies)/1.5;
	}
	rep(i,1,N) rep(j,1,N) if (i!=j && neighbors[i][j] && regions[j].visible && super_regions[regions[i].master].belong==1 && regions[j].belong==2)
		regions[j].rating+=0.2;
	rep(i,1,N) if (regions[i].belong==2) regions[i].rating+=0.1;

	// rep(i,1,N) if (regions[i].visible && regions[i].belong!=1) cout<<"i="<<i<<": "<<regions[i].rating<<endl;
}

void bfs(int uu){
	int l=0,r=1;
	bfs_q[1]=uu;
	regions[uu].transrating=0;
	rep(i,1,N) bfs_f[i]=0;
	while (l<r){
		int u=bfs_q[++l];
		rep(v,1,N) if (neighbors[u][v] && regions[u].transrating+1<regions[v].transrating){
			regions[v].transrating=regions[u].transrating+1;
			if (!bfs_f[v]){
				bfs_f[v]=1;
				bfs_q[++r]=v;
			}
		}
	}
}

void transfer_armies(){
	transfer_list.clear();
	/*rep(i,1,N) if (regions[i].belong==1 && !regions[i].near0 && !regions[i].near2 && regions[i].near1 && regions[i].armies>1){
		int k=rand()%4;
		rep(j,1,N) if (neighbors[i][j] && regions[j].belong==1){
			transfer_list.push_back(attack_transfer(i,j,regions[i].armies-1));
			break;
		}
	}*/
	rep(i,1,N) regions[i].transrating=100;
	rep(i,1,N) if (regions[i].belong==2) bfs(i);
	rep(i,1,N) regions[i].transrating*=-100;
	rep(i,1,N) if (super_regions[regions[i].master].belong==1)
		regions[i].transrating+=super_regions[regions[i].master].bonus;

	rep(i,1,N) if (regions[i].armies>1 && regions[i].belong==1){
		int p=i;
		rep(j,1,N) if (regions[j].belong==1 && neighbors[i][j] && regions[j].transrating>regions[p].transrating) p=j;
		if (p!=i) transfer_list.push_back(attack_transfer(i,p,regions[i].armies-1));
	}
}

bool cmp(int a,int b){return regions[a].rating>regions[b].rating;}

void placeandattack(){
	attack_rank.clear();
	attack_list.clear();
	rep(i,1,N) place_armies[i]=0;
	
	rep(i,1,N) if (regions[i].visible && regions[i].rating>0 && regions[i].belong!=1)
		attack_rank.push_back(i);
	if (attack_rank.empty())
		rep(i,1,N) if (regions[i].visible && regions[i].belong!=1)
			attack_rank.push_back(i);
	sort(attack_rank.begin(),attack_rank.end(),cmp);

	for(auto& i:attack_rank){
		int x=0;
		rep(j,1,N) if (neighbors[i][j] && regions[j].belong==1 && regions[j].armies>regions[x].armies) x=j;
		if (!x) continue;
		int attack_armies=0;
		if (regions[i].belong==0 || regions[i].armies<=5) attack_armies=0.55+regions[i].armies/0.6;
		else if (regions[i].belong==2) attack_armies=0.55+regions[i].armies/0.75;
		if (regions[x].armies<=attack_armies){
			int tmp=min(attack_armies-regions[x].armies+1,income);
			income-=tmp;
			regions[x].armies+=tmp;
			//add_place(x,tmp);
			if (tmp>0) place_armies[x]+=tmp;
			// if (tmp>0) cout<<player1<<" place_armies "<<x<<' '<<tmp<<", ";
		}
		if (regions[x].armies>attack_armies){
			//add_attack(x,i,attack_armies);
			attack_list.push_back(attack_transfer(x,i,attack_armies));
			regions[x].armies-=attack_armies;
		}
	}

	int can_see_player2=0;
	for(auto &x:attack_list) if (regions[x.v].belong==2) can_see_player2=1;
	while (income)
		for(auto &x:attack_list) if (income && (regions[x.v].belong==2 || !can_see_player2)){
			place_armies[x.u]++;
			// cout<<player1<<" place_armies "<<x.u<<' '<<1<<", ";
			regions[x.u].armies++;
			income--;
		}		//多的income加到打敌人身上

	int flag=1;
	while(flag){
		flag=0;
		for(auto &x:attack_list) if (regions[x.u].armies>1 && regions[x.v].belong==2){
			regions[x.u].armies--;
			x.l++;
			flag=1;
		}
	}		//兵有多就拿去打敌人

	flag=1;
	while(flag){
		flag=0;
		for(auto &x:attack_list) if (regions[x.u].armies>1 && !regions[x.u].near2){
			regions[x.u].armies--;
			x.l++;
			flag=1;
		}
	}		//兵还有多而且不用防守就拿去开荒
}

int main(){
	regions[0].transrating=regions[0].rating=-10000;
	string s,command;
	int i,x;
	while (getline(cin,s)){
		stringstream ssin;
		ssin<<s;
		string command;
		ssin>>command;
		if (command=="setup_map"){
			ssin>>s;
			if (s=="super_regions")
				while (ssin>>i>>x) super_regions[i].bonus=x,M=i;
			else if (s=="regions")
				while (ssin>>i>>x){
					regions[i].master=x;
					regions[i].armies=neutral_size;
					regions[i].belong=0;
					super_regions[x].servant.push_back(i);
					N=i;
				}
			else if (s=="neighbors")
				while (ssin>>i){
					ssin>>x;
					neighbors[i][x]=neighbors[x][i]=1;
					while (ssin.peek()==','){
						char ch;
						ssin>>ch>>x;
						neighbors[i][x]=neighbors[x][i]=1;
					}
				}
			else if (s=="wastelands")
				while (ssin>>i){
					regions[i].armies=wasteland_size;
				}
			else if (s=="opponent_starting_regions")
				while (ssin>>i){
					regions[i].belong=2;
					regions[i].armies=initial_armies;
				}
		}
		else if (command=="settings"){
			ssin>>s;
			if (s=="timebank") ssin>>timebank;
			else if (s=="time_per_move") ssin>>time_per_move;
			else if (s=="max_rounds") ssin>>max_rounds;
			else if (s=="your_bot") ssin>>player1;
			else if (s=="opponent_bot") ssin>>player2;
			else if (s=="starting_armies") ssin>>income;
			else if (s=="starting_regions"){
				// while (ssin>>x) starting_regions.push_back(x);
				// sort(starting_regions.begin(),starting_regions.end(),cmp);
			}
		}
		else if (command=="update_map"){
			rep(i,1,N) regions[i].visible=0;
			while (ssin>>i>>s>>x){
				if (s==player1) regions[i].belong=1;
				else if (s==player2) regions[i].belong=2;
				else regions[i].belong=0;
				regions[i].armies=x;
				regions[i].visible=1;
			}
			rep(i,1,N) if (regions[i].visible==0 && regions[i].belong==1) regions[i].belong=2;
		}
		else if (command=="opponent_moves"){
			while(ssin>>s){
				ssin>>s;
				if (s=="place_armies"){
					ssin>>i>>x;
					if (regions[i].visible && regions[i].belong==2 && regions[i].armies>1)
						regions[i].armies+=x;
				}
			}
			//transfer<u,v,l> and v unviseble v+l....  so do <v,u> and attack
		}
		else if (command=="pick_starting_region"){
			renew_map();
			ssin>>s;
			x=0;
			while(ssin>>i) if (regions[i].rating>regions[x].rating) x=i;
			cout<<x<<endl;
		} 
		else if (command=="go"){
			ssin>>s;
			if (s=="place_armies"){
				renew_map();
				placeandattack();
				transfer_armies();
				//rep(i,1,N) cout<<i<<' '<<regions[i].transrating<<endl;
				rep(i,1,N) if (place_armies[i]) cout<<player1<<" place_armies "<<i<<' '<<place_armies[i]<<", ";
				cout<<endl;
			}
			else if (s=="attack/transfer"){
				for(auto& x:transfer_list)
					cout<<player1<<" attack/transfer "<<x.u<<' '<<x.v<<' '<<x.l<<", ";
				for(auto& x:attack_list) if (regions[x.v].belong==0)
					cout<<player1<<" attack/transfer "<<x.u<<' '<<x.v<<' '<<x.l<<", ";
				for(auto& x:attack_list) if (regions[x.v].belong==2)
					cout<<player1<<" attack/transfer "<<x.u<<' '<<x.v<<' '<<x.l<<", ";
				cout<<endl;
			}
		}
		//rep(i,1,N) cout<<i<<' '<<regions[i].belong<<endl;
	}	
}
