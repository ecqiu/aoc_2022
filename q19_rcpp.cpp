#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


////////////////////////////////////////////
// Memoization structure to hold the hash map
struct mem_map{

  // Initializer to create the static (presistent) map
  static std::unordered_map<std::string, double> create_map()
  {
    std::unordered_map<std::string, double> m;
    m.clear();
    return m;
  }

  // Name of the static map for the class
  static std::unordered_map<std::string, double> memo;

};

// Actuall instantiate the class in the global scope (I know, bad me...)
std::unordered_map<std::string, double> mem_map::memo =  mem_map::create_map();



// Reset the map
// [[Rcpp::export]]
void clear_mem(){
  mem_map::memo.clear();
}

// Get the values of the map.
// [[Rcpp::export]]
std::unordered_map<std::string, double> get_mem(){
  return mem_map::memo;
}


////////////////////////////////////////////




// [[Rcpp::export]]
NumericVector min_calc_cpp(double n_geo_req,NumericVector blueprint,double tl_min1,NumericVector robots,NumericVector resources){
  double n_obs_raw;
  double n_obs_req;
  
  double n_clay_raw;
  double n_clay_req;
  
  double n_ore_raw;
  double n_ore_req;
  
  
  n_obs_raw=n_geo_req*blueprint[5]-( (tl_min1-n_geo_req)*robots[2]+resources[2]);
  n_obs_req=std::max(ceil( (n_obs_raw)/(tl_min1-1-n_geo_req) ),0.0);
  if( (tl_min1-1-n_geo_req)<=0){
    if(n_obs_raw<=0){
      n_obs_req=0;
    }else{
      n_obs_req=99999;
    }
  }
  
  n_clay_raw=n_obs_req*blueprint[3]-( (tl_min1-1-n_geo_req)*robots[1]+resources[1]);
  n_clay_req=std::max(ceil( (n_clay_raw)/(tl_min1-2-n_geo_req) ),0.0);
  if( (tl_min1-2-n_geo_req)<=0){
    if(n_clay_raw<=0){
      n_clay_req=0;
    }else{
      n_clay_req=99999;
    }
  }
  
  n_ore_raw=(n_geo_req*blueprint[4]+ n_obs_req*blueprint[2]+n_clay_req*blueprint[1]) -( (tl_min1-n_geo_req)*robots[0]+resources[0]);
  n_ore_req=std::max(ceil( (n_ore_raw)/(tl_min1-1-n_geo_req) ),0.0);
  if( (tl_min1-1-n_geo_req)<=0){
    if(n_ore_raw<=0){
      n_ore_req=0;
    }else{
      n_ore_req=99999;
    }
  }  
  
  NumericVector out= {n_obs_raw,n_obs_req,n_clay_raw,n_clay_req,n_ore_raw,n_ore_req};
  
  return(out);
}

// [[Rcpp::export]]
NumericVector max_calc_cpp(double n_geo_req,NumericVector blueprint,double tl_min1,NumericVector robots,NumericVector resources){
  double n_obs_raw;
  double n_obs_req;
  
  double n_clay_raw;
  double n_clay_req;
  
  double n_ore_raw;
  double n_ore_req;
  
  
  n_obs_raw=n_geo_req*blueprint[5]-(robots[2]+resources[2]);
  n_obs_req=n_obs_raw;
  
  n_clay_raw=n_obs_req*blueprint[3]-(robots[1]+resources[1]);
  n_clay_req=n_clay_raw;
  
  n_ore_raw=(n_geo_req*blueprint[4]+ n_obs_req*blueprint[2]+n_clay_req*blueprint[1]) -( robots[0]+resources[0]);
  n_ore_req=n_ore_raw;
  
  NumericVector out= {n_obs_raw,n_obs_req,n_clay_raw,n_clay_req,n_ore_raw,n_ore_req};
  
  return(out);
}

// [[Rcpp::depends(BH)]]

#include <boost/lexical_cast.hpp>  
using boost::lexical_cast;
using boost::bad_lexical_cast;
std::vector<std::string> lexicalCast(std::vector<double> v) {

    std::vector<std::string> res(v.size());

    for (unsigned int i=0; i<v.size(); i++) {
        try {
            res[i] = lexical_cast<std::string>(v[i]);
        } catch(bad_lexical_cast &) {
            res[i] = "(failed)";
        }
    }

    return res;
}


std::string v_to_str(NumericVector v){
 std::string s;
  for (int i = 0; i < v.length(); i++){
      s += std::to_string(int(v[i]));
      s +="_";
  }
  return s;
}

// [[Rcpp::export]]
double q19_cpp(double time_left,NumericVector robots,NumericVector resources,NumericVector blueprint, double maxc) {
  
  
  std::string key= std::to_string(int(time_left)) +"|"+ v_to_str(blueprint) +"|"+ v_to_str(robots) +"|"+ v_to_str(resources);
  if(maxc <0){
    Rcout << "key: " << key << std::endl;
  }
  
  if(!(mem_map::memo.find(key) == mem_map::memo.end()) ){
    return(mem_map::memo[key]);
  }
  
  
  if(time_left<=0){
    return(resources[3]);
  }
  
  double tl_min1=time_left;
  double n_geo_req=std::min(20.0,time_left-1);
  
  NumericVector calc_out=min_calc_cpp(n_geo_req,blueprint,tl_min1,robots,resources);
  
  double n_obs_raw=calc_out[0];
  double n_obs_req=calc_out[1];
  double n_clay_raw=calc_out[2];
  double n_clay_req=calc_out[3];
  double n_ore_raw=calc_out[4];
  double n_ore_req=calc_out[5];
  
  
  //find max geo robots constructable
  while((n_ore_req+n_clay_req+n_obs_req+n_geo_req) >(tl_min1-1) &n_geo_req>0){
    n_geo_req=n_geo_req-1;
    
    calc_out=min_calc_cpp(n_geo_req,blueprint,tl_min1,robots,resources);
  
    n_obs_raw=calc_out[0];
    n_obs_req=calc_out[1];
    n_clay_raw=calc_out[2];
    n_clay_req=calc_out[3];
    n_ore_raw=calc_out[4];
    n_ore_req=calc_out[5];
  }
  
  
  if(n_geo_req==0){
    n_ore_req=0;
    n_obs_req=0;
    n_clay_req=0;
    n_geo_req=0;
  }else{
    // if((n_ore_req+n_clay_req+n_obs_req+n_geo_req) <tl_min1 ){
    //   while((n_ore_req+n_clay_req+n_obs_req+n_geo_req) <tl_min1 ){
    //     tl_min1=tl_min1-1;
    //     
    //     
    //     calc_out=max_calc_cpp(n_geo_req,blueprint,tl_min1,robots,resources);
    // 
    //     n_obs_raw=calc_out[0];
    //     n_obs_req=calc_out[1];
    //     n_clay_raw=calc_out[2];
    //     n_clay_req=calc_out[3];
    //     n_ore_raw=calc_out[4];
    //     n_ore_req=calc_out[5];
    //   }
    //   tl_min1=tl_min1+1;
    // }
    calc_out=max_calc_cpp(n_geo_req,blueprint,tl_min1,robots,resources);

    n_obs_raw=calc_out[0];
    n_obs_req=calc_out[1];
    n_clay_raw=calc_out[2];
    n_clay_req=calc_out[3];
    n_ore_raw=calc_out[4];
    n_ore_req=calc_out[5];
  }
  
  double temp_val=0;
  
  if( (time_left-1) >= std::max(time_left-n_geo_req,0.0)){
    temp_val=sum(Rcpp::Range(std::max(time_left-n_geo_req,0.0),(time_left-1)));
  }else{
    // temp_val=sum(Rcpp::Range((time_left),std::max(time_left-n_geo_req,0.0)));
    temp_val=0;
  }
  // temp_val=n_geo_req*(time_left);
  
  double max_geo=temp_val+robots[3]*time_left+resources[3];
  
  if(max_geo<=maxc){
    // Rcout << "time_left: " << time_left << std::endl;
    // Rcout << "resources: " << resources << std::endl;
    // Rcout << "robots: " << robots << std::endl;
    // Rcout << "blueprint: " << blueprint << std::endl;
    // Rcout << "max_geo: " << max_geo << std::endl;
    // Rcout << "maxc: " << maxc << std::endl;
    
    return(0);
  }
  
  resources=resources+robots;

  
  double max_temp=0;
  NumericVector possib_geo_out(5);
  
  if((resources[0]-robots[0]) >=blueprint[4]& (resources[2]-robots[2]) >=blueprint[5] &n_geo_req>0 ){
    NumericVector copy_robots=clone(robots);
    copy_robots[3]=copy_robots[3]+1;
    
    NumericVector copy_resources=clone(resources);
    copy_resources[0]=copy_resources[0]-blueprint[4];
    copy_resources[2]=copy_resources[2]-blueprint[5];
    possib_geo_out[0]=q19_cpp(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc);
  }
  max_temp=Rcpp::max(possib_geo_out);
  maxc=std::max(maxc,max_temp);
 
  if((resources[0]-robots[0]) >=blueprint[2]& (resources[1]-robots[1]) >=blueprint[3] &n_obs_req>0 ){
    NumericVector copy_robots2=clone(robots);
    copy_robots2[2]=copy_robots2[2]+1;
    
    NumericVector copy_resources2=clone(resources);
    copy_resources2[0]=copy_resources2[0]-blueprint[2];
    copy_resources2[1]=copy_resources2[1]-blueprint[3];
    possib_geo_out[1]=q19_cpp(time_left-1,copy_robots2,copy_resources2,blueprint,maxc=maxc);
  }
  max_temp=Rcpp::max(possib_geo_out);
  maxc=std::max(maxc,max_temp);
 
  if((resources[0]-robots[0]) >=blueprint[1] &n_clay_req>0 ){
    NumericVector copy_robots3=clone(robots);
    copy_robots3[1]=copy_robots3[1]+1;
    
    NumericVector copy_resources3=clone(resources);
    copy_resources3[0]=copy_resources3[0]-blueprint[1];
    
    possib_geo_out[2]=q19_cpp(time_left-1,copy_robots3,copy_resources3,blueprint,maxc=maxc);
  }
  max_temp=Rcpp::max(possib_geo_out);
  maxc=std::max(maxc,max_temp);
 
  if((resources[0]-robots[0]) >=blueprint[0] &n_ore_req>0 ){
    
    
    
    NumericVector copy_robots4=clone(robots);
    copy_robots4[0]=copy_robots4[0]+1;
    
    NumericVector copy_resources4=clone(resources);
    copy_resources4[0]=copy_resources4[0]-blueprint[0];
    
    // if(time_left >17){
      // Rcout << "possib_geo_out: " << possib_geo_out<< std::endl;
      // Rcout << "resources: " << copy_resources4 << std::endl;
      // Rcout << "robots: " << copy_robots4 << std::endl;
      // Rcout << "tl: " << time_left-1 << std::endl;
      // Rcout << "maxc: " << maxc << std::endl;
    // }
    
    possib_geo_out[3]=q19_cpp(time_left-1,copy_robots4,copy_resources4,blueprint,maxc=maxc);
    
    
  }
  max_temp=Rcpp::max(possib_geo_out);
  maxc=std::max(maxc,max_temp);
 
 
   ////////////////////////////////////////////////
  NumericVector copy_robots5=clone(robots);
  NumericVector copy_resources5=clone(resources);
  
  possib_geo_out[4]=q19_cpp(time_left-1,copy_robots5,copy_resources5,blueprint,maxc=maxc);
  
  double out =Rcpp::max(possib_geo_out);
  mem_map::memo[key]=out;
  return(out);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# timesTwo(42)
*/
